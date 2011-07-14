      module OzcotModule
      Use CropLibrary
      use DataTypes

! ====================================================================
!      ozcot parameters
! ====================================================================

!   Short description:
!      Assorted constants used through out

!   Assumptions:
!      None

!   Notes:

!   Changes:
!      psc - 300394
!      300695 jngh changed max_layer from 11 to 100
!      DPH 7/7/94 Moved title variable out to separate common block.
!                 Essential for UNIX systems.
!      JNGH - 12/7/94 Added dlayr_cm to represent depth in cm

!   Constant values
      character Module_name*(7)       ! Name of this module
      parameter (Module_name='ozcot')

      integer Max_layers
      parameter (Max_layers=100)

      ! Fruit age classes
      integer Small_Sqz
      parameter (Small_Sqz=1)
      integer Medium_Sqz
      parameter (Medium_Sqz=2)
      integer Large_Sqz
      parameter (Large_Sqz=3)
      integer Flowers
      parameter (Flowers=4)
      integer Small_bolls
      parameter (Small_bolls=5)
      integer Medium_bolls
      parameter (Medium_bolls=6)
      integer Large_bolls
      parameter (Large_bolls=7)
      integer Inedible_bolls
      parameter (Inedible_bolls=8)
      integer open_bolls
      parameter (open_bolls=9)

      integer Max_categories
      parameter (Max_categories=9)

      integer Max_cohort
      parameter (Max_cohort=300)

      real   ozcot_kvalue
      parameter (ozcot_kvalue = 1.0)

      integer       sw_sowing
      parameter (sw_sowing = 2)
      integer       rain_preboll
      parameter (rain_preboll = 3)
      integer       rain_postboll
      parameter (rain_postboll = 4)
      integer       rain_fallow
      parameter (rain_fallow = 5)
      integer       rain_pre_post_boll
      parameter (rain_pre_post_boll = 7)
      integer       et_accum
      parameter (et_accum = 8)

!cjh v2001       integer Max_rrig
!cjh v2001       parameter (Max_rrig = 10)

      integer    age1
      parameter (age1 = 1)
      integer    age6
      parameter (age6 = 6)
      integer    age7
      parameter (age7 = 7)

      integer Max_age
      parameter (Max_age = 7)

         ! crop status

      character  status_alive*(*)
      parameter (status_alive = 'alive')

      character  status_dead*(*)
      parameter (status_dead = 'dead')

      character  status_out*(*)
      parameter (status_out = 'out')

      character  crop_inactive*(*)
      parameter (crop_inactive = 'inactive')

      real       c_uptakn_max
      parameter (c_uptakn_max = 240.0)

*   Constant values
      integer    root                  ! root
      parameter (root = 1)

      integer    leaf                  ! leaf
      parameter (leaf = 2)

      integer    stem                  ! stem
      parameter (stem = 3)

      integer    pod                ! pod
      parameter (pod = 4)

      integer    meal                ! meal - excludes oil component
      parameter (meal = 5)

      integer    oil                 ! seed oil
      parameter (oil = 6)

      integer    max_part              ! number of plant parts
      parameter (max_part = 6)


      character(len=*),dimension(max_part), parameter ::
     : part_name = (/'root', 'leaf', 'stem', 'pod ', 'meal', 'oil '/)



! ====================================================================
      type OzcotGlobals
      Sequence

      real    frudw_shed
         real    APPLIED_N
         real    TOTAL_APPLIED

         character TITLE*15
         character plant_status*(10)
!jh         real    DAY(3)
!jh         real    HR(2)
!         real    HAIL_LAG
!         logical hail
         real    WLI
         real    smi_wlog
         real    F_LIMITING
         real    smi_row
         real    smi_pre
         logical USESKIP
         real    TEMPMX
         real    TEMPMN
         real    SOLRAD
         real    RAIN
!jh         real    EPAN
         real    TEMPDY
         real    TEMPWT
         real    WIND
         real    TEMPAV
         real    HUNITS
         real    ASOIL
!jh v2001         real    EOS
         real    QA
         real    SOLRTO
         real    Q
!jh         real    SUMES1
!jh         real    SUMES2
         real    EO
         real    ES
         real    EP
         real    ET
         real    HO
!cjh v2001          real    G
         real    TR
         real    f_intz
!cjh v2001          real    RRIG(Max_rrig)
         real    RTSW
!jh v2001         real    DEFIRG
!                 real    RANSUB(20)
         real    VPD
         real    BPER
         real    dlayr(max_layers)
         real    dlayr_cm(max_layers)
         real    ULLAYR(max_layers)
         real    dullayr(max_layers)
         real    STLAYR(max_layers)
         real    SWLAYR(max_layers)
         real    SW
         real    UL
         real    DUL
         real    sat
         real    BULKD(Max_layers)
!cjh v2001          real    STEMP
!jh v2001         real    TRANS(max_layers)
!jh v2001         real    DEF
         real    WPWC
         real    WHCSUB
         real    ULSUB
         real    AVSWSM
!                 real    SWEPLR(10)
!                 real    SWWEP
!jh v2001         real    TSWL(max_layers)
!jh v2001         real    SETLYR(max_layers)
         real    ESUM
         real    ALAI
         real    ALAI_row
         real    RTDEP
         real    RTGROW
         real    CRSPCE
         real    PPM
         real    SDEPTH
         real    RTDEPM
         real    SHEDLF
         real    SMI
         real    S
         real    RS
         real    PP
         real    PS
         real    FLL
!                 real    SNAPLC(2)
         real    SNAPLC
         real    AVAILN
         real    initialN
!                 real    SOILNT
         real    UPTAKN
         real    VEGN
         real    FRUN
         real    PLANTN
         real    SEED_NC
         real    STRUCN
         real    FRUCAT(Max_categories-1)
         real    DAYSQZ
         real    DAYSFL
         real    DAYSOP
         real    FMKCAT(Max_categories-2,Max_age)
         real    DD
         real    DDMERG
         real    SUMDD
         real    BGRVAR
         real    FRUDW
         real    SQUARZ
         real    BOLLZ
         real    OPENZ
         real    SITES
         real    sites1
         real    SIZE
         real    BLOAD
         real    OPENWT
!                 real    SQCNT(25)
!                 real    BLCNT(25)
!                 real    OPCNT(25)
!                 real    ALCT(25)
!                 real    ANUP(25)
!jh         real    SQCON(10)
!jh         real    RESPCON(10)
!jh         real    FLAI(10)
!jh         real    FCUTOUT(10)
         real    CARCAP
         real    CUTOUT
         real    VNSTRS
         real    FNSTRS
         real    RAD
         real    PCLINT
         real    CARCAP_C
         real    CARCAP_N
!jh         real    SCBOLL(10)
         real    FRUNO(Max_cohort)
         real    FRUWT(Max_cohort)
         real    FRMARK(Max_cohort,Max_age)
         real    FYZAGE(Max_cohort)
         real    DLAI(Max_cohort)
         real    ALAIZ
         real    PLNTNZ
!         real    DEFIRR(20)
         real    TWATER
         real    ALINT
         real    GROSS_MARG
!jh v2001         real    DEF_LAST
         real    SQZX
         real    AGRON_INP
         real    SOILW_INP
         real    COUNT_INP
         real    RAIN_INP
         real    MET_INP
         real    FCOT_OUT
         real    FRUCAL_OUT
         real    YIELD_OUT
         real    s_bed_mi
!         real    s_bed_sat
         real    delay
         real    bpsum(Max_cohort)
         real    BOLLGR
         real    DLAI_POT
         real    DW_BOLL
         real    DW_LEAF
         real    DW_ROOT
         real    DW_STEM
         real    N_BOLL
         real    N_LEAF
         real    N_ROOT
         real    N_STEM
         real    DW_TOTAL
         real    RESERVE
         real    RES_CAP
         real    ROOT_FEEDBACK
         real    dDW_L(Max_cohort)
         real    dDW_BOLL
         real    dDW_LEAF
         real    dDW_ROOT
         real    dDW_ROOT_max
         real    dDW_STEM
         real    LEAF_RES
         real    STEM_RES
         real    ROOT_RES
         real    LEAF_RES_N
         real    STEM_RES_N
         real    ROOT_RES_N
         real    total_n
         real    dn_plant
         real    tsno3
         real    tsnh4
         real    no3mn(max_layers)
         real    nh4mn(max_layers)
         real    yest_tsn
         real    yest_tsno3
         real    yest_tsnh4
         real    ano3(max_layers)
         real    anh4(max_layers)
         real    ppm_target
         real    ppm_row
         real    nskip
         integer NDAY_CO
         integer NWET_CO
         real    SUM_TMX
         real    AVE_TX
         real    DELAY_emerg
         real    DD_EMERG
         real    PPM_SOWN
         real    PPM_EMERGE
         real    PPM_ESTABLISH
         real    FAIL_EMRG
         real    F_DIE
         real    height

         integer Last_Iday
!         integer IMET
         integer IMMO
         integer IMDY
         integer IMYR
         integer JDATE
!cjh v2001          integer MDPY
!         integer IGDAY(20)
!         integer NEWRAN
!         integer NRNSDY(20)
         integer NLAYR
         integer nrtlayr
         integer ISW
         integer IEMRG
         integer ISOW
!         integer ISMO
!         integer ISDY
!         integer ISYR
         integer ISQ
         integer IVAR
!jh         integer IDATE
         integer ILAI
         integer IDAY
!         integer IERMO
!         integer IERDY
!         integer IERYR
!         integer NFERT
!         integer NDAY(2)
         integer LFRU(Max_categories)
!         integer JCO(25)
!         integer ICO(25)
         integer LAST
         integer NEXT
         integer LASTL
         integer NEXTL
         integer IDAYCO
         integer LAST_DAY
         integer ILAIZ
         integer IPLNTN
         integer NIRR
         integer ISQZX
         integer J_PICK
         integer N_PICK
         integer N_DEF
         integer I_DEF
         integer I_DEF2
         integer J_DEF
         integer LAI_INP
         integer das
         integer iend
         integer idayx
         integer lastlf
         integer n_cutout
         integer ifrost
         integer istress
         integer ireliefco
         integer INITIAL
         integer days_since_fert
         logical Crop_in                  ! Is a crop in ?
         logical Zero_variables
!      character  module_name*50      ! module name
      character  crop_class*50       ! crop type
      character  cultivar*20         ! name of cultivar
      real        SFMCAT(Max_categories-1)
      real     sw_start(max_layers)
      integer nsince
          real co2
      end type OzcotGlobals

! ====================================================================

      type OzcotParameters
!
!     20080109 DBJ  added p%BckGndRetn  to allow varietal adjustment for
!                   background retention.
! ---------------------------------------------------------------------
      Sequence

         real    UNUL(max_layers)
         integer num_ll_vals

         real     percent_l
         real     scboll
         real     respcon
         real     sqcon
         real     fcutout
         real     flai
         real     DDISQ
!         real     TIPOUT
         real     dlds_max
         real     POPCON
         real     acotyl
         real     rlai
         real     FRUDD(Max_categories-1)
         real     BLTME(Max_categories-1)
         real     WT(Max_categories-1)
         real     FBURR
         ! emergence
      real    rate_emergence
         real     x_co2_fert(20)
         real     y_co2_fert(20)
         integer  num_co2_fert

         real     x_stem_wt(20)
         real     y_height(20)
         integer  num_height

         real     BckGndRetn

      end type OzcotParameters
! ====================================================================

      type OzcotConstants
      Sequence
      character  crop_type*50        ! crop type
                                             ! reporting
      real       row_spacing_default
      real       nskip_default
!jh         integer MODE
      real leaf_res_n_conc
!jh      real dlds
!      real a
!      real b1
!      real b2
!      real b3
         real    HUCUT
         real    BASET
!jh         real    AMBDA
         real    CONA
         real    UL1
         real    OPEN_DEF
!jh         integer IWINDOW
!jh         real    SOW_SW
         real    A_ROOT_LEAF
         real    A_STEM_LEAF
         real    SPECIFIC_LW
         real    T_OPT
         real    T_BASE
         real    EMBRYO
         real    F_LEAF
         real    F_STEM
         real    F_ROOT
         real    WT_AREA_MAX
!jh         real    WT_AREA_MIN
         real    E_PAR
         real    elevation_default

      real    watlog_c  ! sw/ul ratio for waterlogging
      real    watlog_n  ! for waterlogging
      real    wlog_assimilate_red ! assimilate reduction with waterlogging
      real    wlog_carcap_red ! carrying capacity reduction with waterlogging
      real    wlog_carcap_red_stress  ! carrying capacity reduction with waterlogging after stress
      real    smi_affect_wlog ! level of water stress below which could affect waterlogging
      integer days_relief_wlog ! number of days of stress relief after which has no effect on waterlogging

      real    frost_kill_immediate   ! min temperature below which immediate kill

      real    rtdep_max
      real    harvest_n_frac ! fraction of uptake N for potential N harvested
        !c-----------------------------------------------------------------------------
        !c     if coutout due to water stress or full boll load ie smi < 0.75
        !c     10% of fruiting sites become inactive for frugen every day after 5 days
        !c-----------------------------------------------------------------------------
      real    cutout_smi_crit !
      integer cutout_smi_days
      real    cutout_smi_site_red

      real    smi_delay_crit
      real    cold_shock_delay_crit
      real    cold_shock_delay


      real    epcoef1
      real    epcoef2
      real    epcoef_smi_crit

        ! water stress on bolls
      real    fbwstr_low       ! lower limit
      real    fbwstr_high      ! upper limit
      real    fbwstr_a       ! power

        ! N stress
      real    fbnstr_low
      real    fbnstr_high
      real    fbnstr_a

        ! water stress on photosynthesis
      real    relp_smi_crit     ! critical level of smi below which it affects pp
      real    relp_intercept    ! intercept  of f(smi)
      real    relp_slope        ! slope  of f(smi)

        ! severity of effect of water stress on photosynthesis
      real    relp_low
      real    relp_high
      real    relp_a

        ! N stress on fruiting site number
      real    vsnstr_low
      real    vsnstr_high
      real    vsnstr_a

        ! water stress on pre-squaring lai
      real    flfsmi_low
      real    flfsmi_high
      real    flfsmi_a

        ! N stress on lai
      real    vlnstr_low
      real    vlnstr_high
      real    vlnstr_a

        ! effect of water stress on leaf senescence
      real    fw_low
      real    fw_high
      real    fw_a

        ! plant N adjustment for supply/demand ratio
      real    adjust_low
      real    adjust_high
      real    adjust_a

        ! water stress for boll growth
      real    fwstrs_low
      real    fwstrs_high
      real    fwstrs_a

      real    fert_crit
      real    fert_detect
      integer days_since_fert_max

      end type OzcotConstants
! ====================================================================
      type IDsType
         sequence
         integer :: crop_chopped
         integer :: sowing
         integer :: harvesting
         integer :: create
         integer :: sysinit
         integer :: end_run
         integer :: sow
         integer :: harvest
         integer :: end_crop
         integer :: kill_crop
         integer :: tick
         integer :: newmet
         integer :: prepare
         integer :: process
         integer :: post
         integer :: IncorpFOM
      end type IDsType

      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (OzcotGlobals),pointer :: g
      type (OzcotParameters),pointer :: p
      type (OzcotConstants),pointer :: c
      type (IDsType), pointer :: id

      contains






* ====================================================================
       subroutine ozcot_Init ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Purpose
*      Initialise ozcot module

*+  Changes
*      psc - 9/08/93
*      250996 jngh removed unused includes
*      060599 sdb removed version reference

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_init')

*+  Local Variables
       character Event_string*40       ! String to output

*- Implementation Section ----------------------------------
      call push_routine(myname)
      ! Notify system that we have initialised

      Event_string = 'Initialising'
      call Write_string (Event_string)

      ! Get all parameters from parameter file

      call ozcot_read_constants ()
!jh      call ozcot_read_param ()
      call ozcot_read_root_params ()
!psc      call init()                      ! now called from o_zero_variables
!jh      call ozcot_initial()

      call pop_routine(myname)
      return
      end subroutine



!obsolete * ====================================================================
!obsolete        subroutine ozcot_read_param ()
!obsolete * ====================================================================
!obsolete !obsolete       Use Infrastructure
!      implicit none
!obsolete        include 'const.inc'             ! Constant definitions
!obsolete       include 'read.pub'
!obsolete       include 'error.pub'
!obsolete
!obsolete *+  Purpose
!obsolete *      Read in all parameters from parameter file.
!obsolete
!obsolete *+  Changes
!obsolete *      psc - 09/08/93 first go
!obsolete *      psc - 30/03/94 specified properly
!obsolete *      DPH - 7/7/94  Removed free format internal read to g%title.  Line now
!obsolete *                    reads g%title = param_string
!obsolete *      psc - 15/4/98 Add ll, remove g%title, g%asoil from read
!obsolete *      jngh - 30/4/98 kept numvals of ll read as global
!obsolete *                     made reading of ll optional with a warning error if not found
!obsolete *                    as ll15 will then be used.
!obsolete
!obsolete *+  Constant Values
!obsolete       character  myname*(*)            ! name of subroutine
!obsolete       parameter (myname = 'ozcot_read_param')
!obsolete       character  section_name*(*)
!obsolete       parameter (section_name = 'parameters')
!obsolete
!obsolete *+  Local Variables
!obsolete !       integer numvals
!obsolete
!obsolete *- Implementation Section ----------------------------------
!obsolete       call push_routine(myname)
!obsolete !         ! read in title from parameter file
!obsolete !      call read_char_array (section_name
!obsolete !     :                     , 'title', 15, '()'
!obsolete !     :                     , title, numvals)
!obsolete
!obsolete !         ! read in soil temperature factor from parameter file
!obsolete !      call read_real_var (section_name
!obsolete !     :                    , 'asoil', '()'
!obsolete !     :                     , asoil, numvals
!obsolete !     :                     , 0.0, 10000.0)
!obsolete
!obsolete       call read_real_array_optional (section_name
!obsolete      :                     , 'll', max_layers, '(mm/mm)'
!obsolete      :                     , p%unul, p%num_ll_vals
!obsolete      :                     , 0.0, 1.0)
!obsolete
!obsolete       if (p%num_ll_vals.ne.0) then
!obsolete          ! LL found
!obsolete       else
!obsolete          ! LL not found
!obsolete          call warning_error (err_user
!obsolete      :         , ' Cotton LL not found. Using Soilwat LL15 instead.' )
!obsolete       endif
!obsolete
!obsolete       call pop_routine(myname)
!obsolete       return
!obsolete       end subroutine

*     ===========================================================
      subroutine ozcot_zero_all_globals ()
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Purpose
*       Zero all global variables & arrays

*+  Changes
*     041199 jngh

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'ozcot_zero_all_globals')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      g%frudw_shed = 0.0
      g%snaplc   = 0.0
      c%fert_crit= 0.0
      c%fert_detect= 0.0
      g%APPLIED_N = 0.0
      g%TOTAL_APPLIED = 0.0

      g%TITLE  = ' '
      g%plant_status = status_out
!jh      g%DAY(:)   = 0.0
!jh      g%HR(:)    = 0.0
!      g%HAIL_LAG = 0.0
!      g%HAIL = .false.
      g%wli      = 0.0
      g%F_LIMITING = 0.0
      g%smi_row   = 0.0
      g%smi_pre   = 0.0
      g%USESKIP   = .false.
      g%TEMPMX   = 0.0
      g%TEMPMN   = 0.0
      g%SOLRAD   = 0.0
      g%RAIN     = 0.0
!jh      g%EPAN     = 0.0
      c%HUCUT    = 0.0
      c%BASET    = 0.0
      g%TEMPDY   = 0.0
      g%TEMPWT   = 0.0
      g%WIND     = 0.0
      g%TEMPAV   = 0.0
      g%HUNITS   = 0.0
      g%ASOIL    = 0.0
!jh v2001      g%EOS      = 0.0
      g%QA       = 0.0
      g%SOLRTO   = 0.0
      g%Q        = 0.0
!jh      g%SUMES1   = 0.0
!jh      g%SUMES2   = 0.0
      g%EO       = 0.0
      g%ES       = 0.0
      g%EP       = 0.0
      g%ET       = 0.0
      g%HO       = 0.0
!jh v2001       g%G        = 0.0
      g%TR       = 0.0
!jh v2001       g%RRIG(:) = 0.0
      g%RTSW     = 0.0
!jh v2001      g%DEFIRG   = 0.0
!jh      c%AMBDA    = 0.0
      g%VPD      = 0.0
      g%BPER     = 0.0
      g%dlayr(:)            = 0.0
      g%dlayr_cm(:)         = 0.0
      g%ULLAYR(:)           = 0.0
      g%STLAYR(:)           = 0.0
      g%SWLAYR(:)           = 0.0
      g%sw_start(:)         = 0.0
      g%SW                  = 0.0
      g%UL                  = 0.0
      g%sat                 = 0.0
      c%UL1                 = 0.0
      g%BULKD(:)            = 0.0
!jh v2001       g%STEMP               = 0.0
!jh v2001      g%TRANS(:)            = 0.0
!jh v2001      g%DEF                 = 0.0
      g%WPWC                = 0.0
      g%WHCSUB              = 0.0
      g%ULSUB               = 0.0
      g%AVSWSM              = 0.0
!jh v2001      g%TSWL(:)             = 0.0
!jh v2001      g%SETLYR(:)           = 0.0
      g%ESUM                = 0.0
      g%ALAI                = 0.0
      g%ALAI_row            = 0.0
      g%f_intz              = 0.0
      g%height              = 0.0
      g%RTDEP               = 0.0
      g%RTGROW              = 0.0
      g%CRSPCE              = 0.0
      g%PPM                 = 0.0
      g%PPM_target          = 0.0
      g%PPM_row             = 0.0
      g%SDEPTH              = 0.0
      g%RTDEPM              = 0.0
      g%SHEDLF              = 0.0
      g%SMI                 = 0.0
      g%S                   = 0.0
      g%RS                  = 0.0
      g%PP                  = 0.0
      g%PS                  = 0.0
      g%FLL                 = 0.0
      g%initialN            = 0.0
      g%AVAILN              = 0.0
      g%UPTAKN              = 0.0
      g%VEGN                = 0.0
      g%FRUN                = 0.0
      g%PLANTN              = 0.0
      g%SEED_NC             = 0.0
      g%STRUCN              = 0.0
      g%FRUCAT(:)           = 0.0
      g%DAYSQZ              = 0.0
      g%DAYSFL              = 0.0
      g%DAYSOP              = 0.0
      g%FMKCAT(:,:)         = 0.0
      g%DD                  = 0.0
      g%DDMERG              = 0.0
      g%SUMDD               = 0.0
      g%BGRVAR              = 0.0
      g%FRUDW               = 0.0
      g%SQUARZ              = 0.0
      g%BOLLZ               = 0.0
      g%OPENZ               = 0.0
      g%SITES               = 0.0
      g%sites1              = 0.0
      g%SIZE                = 0.0
      g%BLOAD               = 0.0
      g%OPENWT              = 0.0
      p%SQCON           = 0.0
      p%respcon         = 0.0
      p%POPCON              = 0.0
      p%flai            = 0.0
      p%fcutout         = 0.0
      g%CARCAP              = 0.0
      g%CUTOUT              = 0.0
      g%VNSTRS              = 0.0
      g%FNSTRS              = 0.0
      g%RAD                 = 0.0
      g%PCLINT              = 0.0
      g%CARCAP_C            = 0.0
      g%CARCAP_N            = 0.0
      p%scboll          = 0.0
      p%FBURR               = 0.0
      p%BckGndRetn        = 0.0
      g%FRUNO(:)          = 0.0
      g%FRUWT(:)          = 0.0
      g%FRMARK(:,:)       = 0.0
      g%FYZAGE(:)         = 0.0
      g%DLAI(:)           = 0.0
      g%ALAIZ               = 0.0
      g%PLNTNZ              = 0.0
      g%TWATER              = 0.0
      g%ALINT               = 0.0
      g%GROSS_MARG          = 0.0
!jh v2001      g%DEF_LAST            = 0.0
      g%SQZX                = 0.0
      c%OPEN_DEF            = 0.0
      g%AGRON_INP           = 0.0
      g%SOILW_INP           = 0.0
      g%COUNT_INP           = 0.0
      g%RAIN_INP            = 0.0
      g%MET_INP             = 0.0
      g%FCOT_OUT            = 0.0
      g%FRUCAL_OUT          = 0.0
      g%YIELD_OUT           = 0.0
!jh      c%SOW_SW              = 0.0
      g%s_bed_mi            = 0.0
      g%delay               = 0.0
      g%bpsum(:)          = 0.0
      c%A_ROOT_LEAF         = 0.0
      c%A_STEM_LEAF         = 0.0
      g%BOLLGR              = 0.0
      g%DLAI_POT            = 0.0
      g%N_BOLL             = 0.0
      g%N_LEAF             = 0.0
      g%N_ROOT             = 0.0
      g%N_STEM             = 0.0
      g%DW_BOLL             = 0.0
      g%DW_LEAF             = 0.0
      g%DW_ROOT             = 0.0
      g%DW_STEM             = 0.0
      g%DW_TOTAL            = 0.0
      g%RESERVE             = 0.0
      g%RES_CAP             = 0.0
      g%ROOT_FEEDBACK       = 0.0

      g%INITIAL             = 0
      g%NDAY_CO             = 0
      g%NWET_CO             = 0
      g%SUM_TMX             = 0.0
      g%AVE_TX              = 0.0
      g%DELAY_emerg         = 0.0
      g%DD_EMERG            = 0.0
      g%PPM_SOWN            = 0.0
      g%PPM_EMERGE          = 0.0
      g%PPM_ESTABLISH       = 0.0
      g%FAIL_EMRG           = 0.0
      g%F_DIE               = 0.0


      c%SPECIFIC_LW         = 0.0
      c%WT_AREA_MAX         = 0.0
!jh      c%WT_AREA_MIN         = 0.0
      g%dDW_L(:)          = 0.0
      c%e_par               = 0.0
      c%T_OPT               = 0.0
      c%T_BASE              = 0.0
      c%EMBRYO              = 0.0
      c%F_LEAF              = 0.0
      c%F_STEM              = 0.0
      c%F_ROOT              = 0.0
      g%dDW_BOLL            = 0.0
      g%dDW_LEAF            = 0.0
      g%dDW_ROOT            = 0.0
      g%dDW_ROOT_max        = 0.0
      g%dDW_STEM            = 0.0
      g%LEAF_RES            = 0.0
      g%STEM_RES            = 0.0
      g%ROOT_RES            = 0.0
      g%LEAF_RES_N          = 0.0
      g%STEM_RES_N          = 0.0
      g%ROOT_RES_N          = 0.0
      g%total_n             = 0.0
      g%dn_plant            = 0.0
      g%tsno3               = 0.0
      g%tsnh4               = 0.0
      g%no3mn(:)            = 0.0
      g%nh4mn(:)            = 0.0
      g%yest_tsno3          = 0.0
      g%yest_tsn            = 0.0
      g%yest_tsnh4          = 0.0
      g%ano3(:)             = 0.0
      g%anh4(:)             = 0.0
      g%Last_Iday           = 0
!jh      c%MODE                = 0
      g%IMMO                = 0
      g%IMDY                = 0
      g%IMYR                = 0
      g%JDATE               = 0
!jh v2001       g%MDPY                = 0
      g%NLAYR               = 0
      g%NrtLAYR             = 0
      g%ISW                 = 0
      g%IEMRG               = 0
      g%ISOW                = 0
      g%ISQ                 = 0
!jh      g%IDATE               = 0
      g%ILAI                = 0
      g%IDAY                = 0
      g%LFRU(:)             = 0
      g%LAST                = 0
      g%NEXT                = 0
      g%LASTL               = 0
      g%NEXTL               = 0
      g%IDAYCO              = 0
      g%LAST_DAY            = 0
      g%ILAIZ               = 0
      g%IPLNTN              = 0
      g%NIRR                = 0
      g%ISQZX               = 0
      g%J_PICK              = 0
      g%N_PICK              = 0
      g%N_DEF               = 0
      g%I_DEF               = 0
      g%I_DEF2              = 0
      g%J_DEF               = 0
      g%LAI_INP             = 0
!jh      c%IWINDOW             = 0
      g%das                 = 0
      g%days_since_fert     = 0
      g%iend                = 0
      g%idayx               = 0
      g%lastlf              = 0
      g%n_cutout            = 0
      g%ifrost              = 0
      g%istress             = 0
      g%ireliefco           = 0
      g%Crop_in             = .false.     ! Is a crop in ?
      g%Zero_variables      = .false.
      g%sfmcat(:)           = 0.0
      g%nsince = 0
      g%INITIAL = 0
      g%nskip   = 0.0
      p%x_stem_wt(:) = 0.0
      p%y_height(:) = 0.0
      p%num_height = 0
      p%x_co2_fert(:) = 0.0
      p%y_co2_fert(:) = 0.0
      p%num_co2_fert = 0
      
      c%row_spacing_default           = 0.0
      c%elevation_default   = 0.0

      c%watlog_c                = 0.0
      c%watlog_n                = 0.0
      c%wlog_assimilate_red        = 0.0
      c%wlog_carcap_red            = 0.0
      c%wlog_carcap_red_stress     = 0.0
      c%smi_affect_wlog            = 0.0
      c%days_relief_wlog           = 0
      c%frost_kill_immediate       = 0.0
      c%rtdep_max                  = 0.0
      c%harvest_n_frac             = 0.0
      c%cutout_smi_crit            = 0.0
      c%cutout_smi_days            = 0
      c%cutout_smi_site_red        = 0.0
      c%epcoef1                    = 0.0
      c%epcoef2                    = 0.0
      c%epcoef_smi_crit            = 0.0
      c%fbwstr_low                 = 0.0
      c%fbwstr_high                = 0.0
      c%fbwstr_a                   = 0.0
      c%fbnstr_low                 = 0.0
      c%fbnstr_high                = 0.0
      c%fbnstr_a                   = 0.0
      c%relp_smi_crit              = 0.0
      c%relp_intercept             = 0.0
      c%relp_slope                 = 0.0
      c%relp_low                   = 0.0
      c%relp_high                  = 0.0
      c%relp_a                     = 0.0
      c%vsnstr_low                 = 0.0
      c%vsnstr_high                = 0.0
      c%vsnstr_a                   = 0.0
      c%flfsmi_low                 = 0.0
      c%flfsmi_high                = 0.0
      c%flfsmi_a                   = 0.0
      c%vlnstr_low                 = 0.0
      c%vlnstr_high                = 0.0
      c%vlnstr_a                   = 0.0
      c%fw_low                     = 0.0
      c%fw_high                    = 0.0
      c%fw_a                       = 0.0
      c%adjust_low                 = 0.0
      c%adjust_high                = 0.0
      c%adjust_a                   = 0.0
      c%fwstrs_low                 = 0.0
      c%fwstrs_high                = 0.0
      c%fwstrs_a                   = 0.0
      c%smi_delay_crit               = 0.0
      c%cold_shock_delay_crit        = 0.0
      c%cold_shock_delay             = 0.0
      p%rate_emergence               = 0.0
      c%nskip_default                = 0.0
      c%days_since_fert_max          = 0
      ! OzcotParameters

      p%UNUL(:)           = 0.0
      p%num_ll_vals       = 0
      p%num_co2_fert      = 0


      call pop_routine (my_name)
      return
      end subroutine


* ====================================================================
       subroutine ozcot_zero_variables ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Purpose
*     Set all variables in this module to zero.

*+  Changes
*      psc - 9/08/93

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_zero_variables')

*- Implementation Section ----------------------------------
      call push_routine(myname)

      call ozcot_initial()
      g%APPLIED_N = 0.
      g%TOTAL_APPLIED = 0.

      g%frudw_shed = 0.0
      g%das = 0
      g%delay = 0.0
      g%idayx = 0
      p%num_ll_vals = 0
      g%crop_in = .false.
      g%iend = 0
      g%zero_variables = .false.

      g%sumdd         = 0.0
      g%sites         = 0.0
      g%squarz        = 0.0
      g%bollz         = 0.0
      g%openz         = 0.0
      g%alint         = 0.0
      g%bload         = 0.0
      g%frun          = 0.0
      g%carcap_c      = 0.0
      g%carcap_n      = 0.0
      g%vnstrs        = 0.0
      g%fnstrs        = 0.0
      g%dw_total      = 0.0
      g%total_n       = 0.0
      g%alint         = 0.0
      g%alai          = 0.0
      g%tr            = 0.0
      g%f_intz        = 0.0
      g%height        = 0.0
      g%availn        = 0.0
      g%uptakn        = 0.0
      g%tsno3         = 0.0
      g%yest_tsno3    = 0.0
      g%tsnh4         = 0.0
      g%yest_tsnh4    = 0.0
      g%dn_plant      = 0.0
      g%rtdep         = 0.0
      g%s_bed_mi      = 0.0
      g%smi           = 0.0
      g%wli           = 0.0
      g%ep            = 0.0
      g%eo            = 0.0
      g%et            = 0.0
      g%openwt        = 0.0
      g%sqzx          = 0.0
      g%alaiz         = 0.0

      call pop_routine(myname)
      return
      end subroutine



* ====================================================================
       subroutine ozcot_manager (Event_action, event_data)
* ====================================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      character Event_action*(*)       ! (INPUT) Action to be performed
      character Event_data*(*)         ! (INPUT) Data sent with event

*+  Purpose
*     The manager has sent an event to this module.  Process it.

*+  Notes
*     Event_action is the action specified in the management parameter
*     file.  e.g%g. 'sow'

*+  Changes
*      psc - 9/08/93
*      07/07/94 - jngh changed residue module reference to global_active
*      170895 jngh changed message send to message pass to module
*      250996 jngh changed to post_ construct
*      191200 dph  changed from unknown_module to all_active_modules
*                  unknown_module not supported in APSIM2.

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_manager')

*+  Local Variables
       real    res_dm                  ! Residue dry weight (kg/ha)
       real    res_N                   ! Amount of N in residue (kg/ha)

*- Implementation Section ----------------------------------
      call push_routine(myname)
      ! **** Repeat for each action

      if (Event_action .eq. 'sow') then

         call fatal_error(err_internal,
     :                    'Anachronistic call to ozcot_manager!')

      else if (Event_action .eq. 'harvest') then

         ! Report the event to the rest of the system

         call Write_string (Event_action)

         call ozcot_harvest_report ()
         call ozcot_harvest_update ()

      else
         ! Don't know about this event !!!

      endif


      call pop_routine(myname)
      return
      end subroutine



*     ===========================================================
      subroutine ozcot_sow (myrecd)
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      character  myrecd*(*)            ! (INPUT) message received

*+  Purpose
*       start crop using parameters specified in passed record

*+  Changes
*       300394 psc  taken from cm_sat module

*+  Constant Values
      character  myname*(*)            ! procedure name
      parameter (myname  = 'ozcot_sow')

*+  Local Variables
!psc      character  cv_name*20            ! name of cultivar
      character  string*300            ! output string

*- Implementation Section ----------------------------------

      call push_routine (myname)

      if (myrecd.ne.blank) then
         call publish_null(id%sowing)

         ! variety,seed depth,rowspace,plants per m row

         read (myrecd,*) g%ivar,g%sdepth,g%rs,g%ppm

         g%isow = g%jdate
         g%rtdep=g%sdepth
         g%ppm = g%ppm/g%rs  !  adjust for non standard rows incl skip
         g%pp= g%ppm*g%rs
         g%ps=(1.0/g%rs)/g%pp
         g%s=g%ps/g%rs
!jh v2001          g%rrig(sw_sowing) = g%sw               ! soil water at sowing
         g%iend = 0
         g%plant_status = status_alive

             ! report

         write (string, '(a)')
     :                  ' sowing  depth plants row sp'
         call write_string (string)

         write (string, '(a)')
     :                  ' g%day no   mm     m^2    m  '
         call write_string (string)

         write (string, '(i7, 3f7.1, 1x, a10)')
     :                   g%isow, g%sdepth, g%pp, g%rs
         call write_string (string)

         call write_string (blank)

!pcs                 ! get cultivar parameters

!psc         call cm_cultv (cv_name)

      else
            ! report empty sowing record
         call fatal_error (err_user, 'No sowing criteria supplied')

      endif

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine ozcot_get_other_variables ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Purpose
*      Get the values of variables from other modules

*+  Changes
*      psc - 9/08/93
*      psc - 28/03/94  used this routine properly
*      DPH - 7/7/94 Changed call to nt_fac to ozcot_nt_fac
*                   Added check for N module.  If N module doesn't
*                   exist then seed no3ppm with some high values.
*      DPH - 11/7/94   Fixed bug in detection of existence of N module.
*      JNGH - 12/7/94 Changed dlayer in cm to g%dlayr_cm
*      psc - commented out read of LL_DEP

*+  Calls
                                       ! function

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_get_other_variables')

*+  Local Variables
      logical N_in_system              ! Is there any N in system ?
      integer layer                    ! layer number
      real    sat(max_layers)        ! saturated moisture content in layer
      real    dul(max_layers)        ! saturated moisture content in layer
      real    ll(max_layers)        ! saturated moisture content in layer
      real    sat_adj(max_layers)        ! saturated moisture content in layer
      real    ll_adj(max_layers)        ! saturated moisture content in layer
      real    no3(max_layers)        ! soil nitrate kg/ha in layer
      real    nh4(max_layers)        ! soil ammonium kg/ha in layer
      real    urea(max_layers)        ! soil ammonium kg/ha in layer
      integer numvals

*- Implementation Section ----------------------------------
      call push_routine(myname)

      sat(:) = 0.0
      dul(:) = 0.0
      ll(:) = 0.0
      sat_adj(:) = 0.0
      ll_adj(:) = 0.0
      no3(:) = 0.0
      nh4(:) = 0.0
      urea(:) = 0.0
      dul(:) = 0.0
      sat(:) = 0.0


      ! Get depths of each layer

                                ! get depth of each soil water layer
      call get_real_array (unknown_module, 'dlayer', max_layers
     :                                    , '(mm)'
     :                                    , g%dlayr, g%nlayr
     :                                    , 0.0, 1000.0)

      ! Get moist bulk density
      call get_real_array (unknown_module, 'bd', max_layers
     :                                    , '(g/cm3)'
     :                                    , g%bulkd, numvals
     :                                    , 0.0, 10.0)

      if (p%num_ll_vals .eq.0) then
         ! Get unavailable g%sw - use ll15 because crop ll is unavailable
         call get_real_array (unknown_module, 'll15', max_layers
     :                                    , '(mm/mm)'
     :                                    , p%unul, numvals
     :                                    , 0.0, 1.0)
      else
         ! ll had been read at init
      endif

      ! Get upper limit of available g%sw
      call get_real_array (unknown_module, 'dul', max_layers
     :                                    , '(mm/mm)'
     :                                    , dul, numvals
     :                                    , 0.0, 1.0)

      ! Get upper limit of available g%sw
      call get_real_array (unknown_module, 'sat', max_layers
     :                                    , '(mm/mm)'
     :                                    , sat, numvals
!jh     :                                    , g%stlayr, numvals
     :                                    , 0.0, 1.0)

      ! Convert field capacity relative to wilting point.
      ! convert units to cm and cm/cm

      g%rtdepm=0.0
      g%ul=0.0
!pc
      g%sat = 0.0
      g%wpwc=0.0

!      ullayr(j) = ullayr(j)*2.      !   simulate skip row
!      unul(j)   = unul(j)*2.        !          ditto

      do 10 Layer = 1, g%nlayr
         g%dlayr_cm(layer)=g%dlayr(layer)/10.
!         sat_adj(layer) = sat(layer)
!     :           - (sat(layer)-dul(layer))
!     :           * 0.54
!jh         p%unul(layer) = p%unul(layer) * g%dlayr_cm(layer)
         sat_adj(Layer) = (dul(Layer)-p%unul(Layer))/0.87
     :                 + p%unul(Layer) ! adjust to match ozcot soil characterisation
         sat_adj(Layer) = min(sat_adj(layer), sat(layer))
!         ll_adj(Layer) = sat(layer) - sat_adj(layer) + ll(layer)
!jh         g%ullayr(Layer) = sat_adj(layer) - ll(layer)
         g%ullayr(Layer) = sat_adj(layer) - p%unul(layer)
!jh         g%ullayr(Layer) = min(g%ullayr(Layer)
!jh     :                        , sat(Layer)-p%unul(Layer)) ! avoid going above sat

!jh         g%ullayr(layer) = g%ullayr(layer) / g%dlayr(layer)
!jh         g%stlayr(Layer) = g%stlayr(Layer) - p%unul(Layer)
!jh         g%stlayr(layer) = g%stlayr(layer) / g%dlayr(layer)
!jh         p%unul(layer) = p%unul(layer) / g%dlayr(layer)

         g%rtdepm=g%rtdepm+g%dlayr_cm(layer)       ! depth of profile
         g%ul=g%ul+g%ullayr(layer)*g%dlayr_cm(layer) ! upper limit for profile
!jh         g%sat=g%sat+g%stlayr(layer)*g%dlayr_cm(layer) ! saturated limit for profile
         g%wpwc=g%wpwc+p%unul(layer)*g%dlayr_cm(layer) ! unavailable water content
10    continue

      call get_real_var (unknown_module, 'es', '(mm)'
     :                                  , g%es, numvals
     :                                  , 0.0, 1000.0)

      g%es = g%es / 10.                        ! convert to cm

      call get_real_var (unknown_module, 'runoff', '(mm)'
     :                                  , g%q, numvals
     :                                  , 0.0, 1000.0)


      call get_real_array (unknown_module, 'sw', max_layers, '(mm/mm)'
     :                     , g%swlayr, g%nlayr
     :                     , 0.0, 1.0)
      ! Convert water to plant available  (cm/cm)

      do 12 Layer = 1, g%nlayr
!jh        g%swlayr(Layer) = g%swlayr(Layer) - ll_adj(Layer)
        g%swlayr(Layer) = g%swlayr(Layer) - p%unul(Layer)
        g%swlayr(Layer) = max(0.0, g%swlayr(Layer))
        g%sw_start(layer) = g%swlayr(Layer)
12    continue
      g%s_bed_mi = g%swlayr(1)/g%ullayr(1)        ! seed bed moisture index

      !   get initial estimate of available soil no3
      call get_real_array (unknown_module, 'no3_min', max_layers
     :                                    , '(kg/ha)'
     :                                    , g%no3mn, numvals
     :                                    , 0.0, 1000.0)

      call get_real_array_optional (unknown_module, 'no3'
     :                                  , max_layers
     :                                  ,'(kg/ha)'
     :                                  , no3, numvals
     :                                  , 0.0, 1000.0)

      !   get initial estimate of available soil nh4
      call get_real_array (unknown_module, 'nh4_min', max_layers
     :                                    , '(kg/ha)'
     :                                    , g%nh4mn, numvals
     :                                    , 0.0, 1000.0)

      call get_real_array_optional (unknown_module, 'nh4'
     :                                  , max_layers
     :                                  ,'(kg/ha)'
     :                                  , nh4, numvals
     :                                  , 0.0, 1000.0)

      call get_real_array_optional (unknown_module, 'urea'
     :                                  , max_layers
     :                                  ,'(kg/ha)'
     :                                  , urea, numvals
     :                                  , 0.0, 1000.0)

      ! Need to check for situation of no N in system.

      N_in_system = (numvals .eq. g%nlayr)

      if (N_in_system) then
         !nothing

      else
         ! There is no N model in system.  Feed ozcot 150 units of N
         ! distributed throughout the profile

         do 15 Layer = 1, g%nlayr

            no3(Layer) = (150. / g%nlayr * 100.) / g%dlayr_cm(Layer)
15       continue
      endif

      ! Sum soil nitrate over all layers  (kg/ha)

      g%tsno3 = 0.
      do 20 Layer = 1, g%nlayr
        g%ano3(layer) = no3(layer)-g%no3mn(layer)
        g%tsno3 = g%tsno3 + g%ano3(layer)
20    continue

      ! Sum soil ammonimum over all layers  (kg/ha)

      g%tsnh4 = 0.
      do 21 Layer = 1, g%nlayr
        g%anh4(layer) = nh4(layer)-g%nh4mn(layer)
        g%tsnh4 = g%tsnh4 + g%anh4(layer)
21    continue
!pc
!jh         g%availn = g%tsno3 + g%total_n*10.0
!jh         g%availn = g%tsno3 + g%tsnh4

!jh      if (.not. N_in_system) then
!jh         g%availn = g%tsno3
!jh
!jh      else if(g%yest_tsno3.ne.0.) then
!jh         g%availn = g%availn + g%tsno3 - g%yest_tsno3
!jh
!jh      else
!jh         g%availn = g%availn
!jh      endif

      if ((sum(no3(:)) + sum(nh4(:)) + sum(urea(:))
     :   .ge. g%yest_tsn+c%fert_detect) .and. g%yest_tsn .gt. 0.0) then   ! is there a fertiliser application?
            if (.not. g%crop_in) then
               g%availn = g%yest_tsno3
               g%SNAPLC = 0.0
               g%TOTAL_APPLIED = 0.0
               g%APPLIED_N = 0.0
               g%days_since_fert = 0
            else
            endif
      else
      endif

      if (.not. g%crop_in
     :    .and. g%days_since_fert .gt. c%days_since_fert_max) then
         g%availn = 0.0
         g%SNAPLC = 0.0
         g%TOTAL_APPLIED = 0.0
         g%APPLIED_N = 0.0
         g%days_since_fert = 0
      else
         g%days_since_fert = g%days_since_fert + 1
      endif

      if (g%availn .le. 0.0001 .and. g%crop_in) then
         g%availn = g%yest_tsno3     !  There has been no fertiliser event before sowing
      else
      endif

         !jhnote may be better to use sums from above.
         !jhnote need to put a limit of 40 days on this before sowing.

      if (g%tsno3 .gt. g%yest_tsno3 .and. g%yest_tsno3 .gt. 0.0) then
         g%snaplc = g%tsno3 - g%yest_tsno3   ! a possible fertiliser event
         if (g%snaplc .ge. c%fert_crit .and. g%availn .gt. 0.0) then
            ! it is a fertiliser event or result of a fertiliser event
         else
            g%snaplc = 0.0       ! not a fertiliser event
         endif
      else
         g%snaplc = 0.0          ! not a fertiliser event
      endif

      g%yest_tsn = sum(no3(:)) + sum(nh4(:)) + sum(urea(:))

      call get_real_var_optional (unknown_module, 'co2', '(mm)'
     :                                  , g%co2, numvals
     :                                  , 0.0, 1000.0)
      if (numvals .eq. 0) then
         g%co2 = 350.0
      endif
      call pop_routine(myname)
      return
      end subroutine



* ====================================================================
       subroutine ozcot_set_other_variables ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Purpose
*     Update variables owned by other modules.

*+  Changes
*      psc - 9/08/93
*      psc   300394  specified properly
*      DPH   7/7/94  Put 0.0 in max function call instead of 0
*                   Changed call to nt_fac to ozcot_nt_fac
*                    Changed call to set_variable_value to call to
*                    Set_real_array
*      JNGH 18/7/94 Corrected conversion of min no3 from g%ppm to kg/ha
*      JNGH - 12/7/94 Changed dlayer in cm to g%dlayr_cm

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_set_other_variables')

*+  Local Variables
      integer Layer                    ! Layer number
      real    sno3(max_layers)       ! available soil nitrate in layer kg/ha
      real    snh4(max_layers)       ! available soil ammonium in layer kg/ha
      real    dlt_no3(max_layers)    ! soil NO3 uptake in kg/ha
      real    dlt_nh4(max_layers)    ! soil Nh4 uptake in kg/ha
      real    dlt_sw_dep(max_layers) ! soil water uptake in layer mm
      real    trtsno3                ! total no3 in root zone.
      real    trtsnh4                ! total nh4 in root zone.
!jh      real    sw_dep(max_layers) ! soil water uptake in layer mm
      real tempsum

*- Implementation Section ----------------------------------
      call push_routine(myname)
      ! Convert water from plant available  (cm/cm)

      dlt_sw_dep(:) = 0.0
      dlt_no3(:)    = 0.0
      dlt_nh4(:)    = 0.0
!jh      sw_dep(:) = 0.0

      do 10 Layer = 1, g%nlayr
        dlt_sw_dep(Layer) = (g%swlayr(Layer)- g%sw_start(layer))
     :                       * g%dlayr_cm(layer)*10.0
     :
        dlt_sw_dep(Layer) = min(0.0, dlt_sw_dep(Layer))
         call bound_check_real_var (dlt_sw_dep(Layer)
     :                           , -g%sw_start(layer)
     :                              * g%dlayr_cm(layer)*10.0
     :                           , 0.0
     :                           , 'dlt_sw_dep(Layer)')

10    continue

      ! Send updated soil water

!jh      call set_real_array('sw_dep', swlayr, max_layers, '(mm)')
      call Set_real_array (unknown_module, 'dlt_sw_dep', '(mm)'
     :                    , dlt_sw_dep, g%nlayr)
!jh      call get_real_array (unknown_module, 'sw_dep', max_layers, '(mm)'
!jh     :                     , sw_dep, g%nlayr
!jh     :                     , 0.0, 1000.0)

!jh      do 15 Layer = 1, g%nlayr
!jh        g%swlayr(Layer) = (g%swlayr(Layer) + p%unul(Layer))*10.
!jh     :                * g%dlayr_cm(layer)
!jh        g%swlayr(Layer) = max(0.0, g%swlayr(Layer)) - sw_dep(layer)
!jh15    continue
!jh      write(100,*) sum(g%swlayr)
      ! Send updated soil water

!      call Set_real_array (unknown_module, 'sw_dep', '(mm)'
!     :                    , g%swlayr, g%nlayr)
      ! extract soil NO3

      trtsno3 = sum(g%ano3(1:g%nrtlayr))
      trtsnh4 = 0.0
      g%dn_plant = u_bound (g%dn_plant, (trtsno3 + trtsnh4)/10.0)
!jh      trtsnh4 = sum(g%anh4(1:g%nrtlayr))
!      print*,'g%total_n, g%uptakn/10., g%availn,trtsno3 ,g%dn_plant*10.'
!      print*, g%total_n, g%uptakn/10., g%availn,trtsno3 ,g%dn_plant*10.
      do 20 Layer = 1, g%nlayr
         if (trtsno3+ trtsnh4 .gt. 0.0 .and. Layer.le.g%nrtlayr) then
            dlt_no3(Layer) = -g%dn_plant*10. * g%ano3(Layer)
     :                     / (trtsno3 + trtsnh4)
!jh            dlt_nh4(Layer) = -g%dn_plant*10. * g%anh4(Layer)
!jh     :                     / (trtsno3 + trtsnh4)
            dlt_no3(layer) = min(0.0, dlt_no3(layer))
!      print*, dlt_no3(layer), g%ano3(Layer)
         else
            dlt_no3(layer) = 0.0
            dlt_nh4(layer) = 0.0
         endif
         g%ano3(Layer) = g%ano3(Layer) + dlt_no3(Layer)
!         if (g%ano3(Layer) < -1.0) pause
         g%ano3(Layer) = max(0.0, g%ano3(Layer))
         sNO3(layer) = g%ano3(layer) + g%no3mn(layer)
!jh         g%anh4(Layer) = g%anh4(Layer) + dlt_nh4(Layer)
!jh         g%anh4(Layer) = max(0.0, g%anh4(Layer))
!jh         sNh4(layer) = g%anh4(layer) + g%nh4mn(layer)
20    continue
!jh      if (trtsno3+ trtsnh4 .gt. 0.0) then
!jh         g%yest_tsno3 = g%tsno3 - (g%dn_plant*10.)
!jh     :                       * trtsno3 /(trtsno3+trtsnh4)
!jh         g%yest_tsnh4 = g%tsnh4 - (g%dn_plant*10.)
!jh     :                       * trtsnh4 /(trtsno3+trtsnh4)
!jh      else
!jh         g%yest_tsno3 = 0.0
!jh         g%yest_tsnh4 = 0.0
!jh       endif
      g%yest_tsno3 = g%tsno3
      ! Send updated soil N


!jh      call set_real_array('no3', sno3, nlayr, '(kg/ha)' )
!jh      call Set_real_array (unknown_module, 'no3', '(kg/ha)'
!jh     :                    , sno3, g%nlayr)
      call Set_real_array (unknown_module, 'dlt_no3', '(kg/ha)'
     :                    , dlt_no3, g%nlayr)
!jh      call Set_real_array (unknown_module, 'dlt_nh4', '(kg/ha)'
!jh     :                    , dlt_nh4, g%nlayr)

      call pop_routine(myname)
      return
      end subroutine



* ====================================================================
       subroutine ozcot_Send_my_variable
     .    (Variable_name)
* ====================================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
       character Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*      Return the value of one of our variables to caller

*+  Changes
*      psc - 9/08/93
*      DPH 7/7/94 Changed g%crop_in variable to ozcot_crop_in.
*      250996 jngh added message_unused to else block
*                  replaced litteral names to variable (in arguments)
*                  removed unused include

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_send_my_variable')

*+  Local Variables
      real    yield                    ! lint yield kg/ha
      real    dm                       ! total dry matter kg/ha
      real    totnup                   ! N uptake kg/ha
      real    d_nup                    ! daily N uptake kg/ha
      real    bollsc
      real    cover
      real       dm_crop(max_part)           ! dry matter of crop (kg/ha)
      real       dm_N(max_part)              ! N content of dry matter (kg/ha)

*- Implementation Section ----------------------------------
      call push_routine(myname)
      ! **** Repeat for each variable

      if (Variable_name .eq. 'das') then
         call respond2get_integer_var (variable_name
     :        , '(days)', g%das)

      else if (variable_name .eq. 'crop_type') then
         call respond2get_char_var (variable_name
     :        , '()', c%crop_type)

      else if (variable_name .eq. 'sumdd') then
         call respond2get_real_var (variable_name
     :        , '(oCd)', g%sumdd)

      else if (Variable_name .eq. 'sites') then
         call respond2get_real_var (variable_name
     :        , '(1/m2)', g%sites)

      else if (Variable_name .eq. 'squarz') then
         call respond2get_real_var (variable_name
     :        , '(1/m2)', g%squarz)

      else if (Variable_name .eq. 'fru_no_cat') then
         call respond2get_real_array (variable_name
     :        , '(1/m2)', g%frucat, Max_categories)

      else if (Variable_name .eq. 'bollz') then
         call respond2get_real_var (variable_name
     :        , '(1/m2)', g%bollz)

      else if (Variable_name .eq. 'openz') then
         call respond2get_real_var (variable_name
     :        , '(1/m2)', g%openz)

      else if (Variable_name .eq. 'alint') then
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', g%alint)

      else if (Variable_name .eq. 'openwt') then
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', g%openwt*10.0)

      else if (Variable_name .eq. 'frudw') then
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', g%frudw*10.0)

      else if (Variable_name .eq. 'frudw_tot') then
         dm = (g%frudw + g%openwt) * 10.0
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', dm)

      else if (Variable_name .eq. 'frudw_shed') then
         dm = g%frudw_shed * 10.0
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', dm)

      else if (Variable_name .eq. 'frun') then
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', g%frun*10.0)

      else if (Variable_name .eq. 'bload') then
         call respond2get_real_var (variable_name
     :        , '()', g%bload)

      else if (Variable_name .eq. 'carcap_c') then
         call respond2get_real_var (variable_name
     :        , '()', g%carcap_c)

      else if (Variable_name .eq. 'carcap_n') then
         call respond2get_real_var (variable_name
     :        , '()', g%carcap_n)

      else if (Variable_name .eq. 'vnstrs') then
         call respond2get_real_var (variable_name
     :        , '()', g%vnstrs)

      else if (Variable_name .eq. 'fnstrs') then
         call respond2get_real_var (variable_name
     :        , '()', g%fnstrs)

      else if (Variable_name .eq. 'dm') then
         dm = g%dw_total * 10.
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', dm)

      else if (Variable_name .eq. 'dw_boll') then
         dm = g%dw_boll * 10.
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', dm)

      else if (Variable_name .eq. 'dw_root') then
         dm = g%dw_root * 10.
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', dm)

      else if (Variable_name .eq. 'dw_leaf') then
         dm = g%dw_leaf * 10.
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', dm)

      else if (Variable_name .eq. 'dw_stem') then
         dm = g%dw_stem * 10.
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', dm)

      else if (Variable_name .eq. 'totnup') then
         totnup = g%total_n * 10.
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', totnup)

      else if (variable_name .eq. 'yield') then
         yield = g%alint / 227.
         call respond2get_real_var (variable_name
     :        , '(bales/ha)', yield)

      else if (variable_name .eq. 'lint_yield') then
         yield = g%alint / 227.
         call respond2get_real_var (variable_name
     :        , '(bales/ha)', yield)

      else if (variable_name .eq. 'lai') then
         call respond2get_real_var (variable_name
     :        , '(m^2/m^2)', g%alai)

      elseif (variable_name .eq. 'cover_green') then
         if (g%plant_status .eq. status_alive) then
            cover = l_bound (1.0 - g%tr, 0.0)
         else 
            cover = 0.0
         endif   
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , cover)

      elseif (variable_name .eq. 'cover_tot') then
         if (g%plant_status .eq. status_alive) then
            cover = g%f_intz
         else 
            cover = 0.0
         endif   
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , cover)

      elseif (variable_name .eq. 'height') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g%height)

      else if (Variable_name .eq. 'availn') then
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', g%availn)

      else if (Variable_name .eq. 'uptakn') then
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', g%uptakn)

      else if (Variable_name .eq. 'tsno3') then
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', g%tsno3)

      else if (Variable_name .eq. 'ysno3') then
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', g%yest_tsno3)

      else if (Variable_name .eq. 'tsnh4') then
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', g%tsnh4)

      else if (Variable_name .eq. 'ysnh4') then
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', g%yest_tsnh4)

      else if (Variable_name .eq. 'n_uptake') then
         d_nup = g%dn_plant * 10.
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', d_nup)

      else if (variable_name .eq. 'rtdep') then
         call respond2get_real_var (variable_name
     :        , '(cm)', g%rtdep)

      else if (variable_name .eq. 's_bed_mi') then
         call respond2get_real_var (variable_name
     :        , '()', g%s_bed_mi)

      else if (variable_name .eq. 'smi') then
         call respond2get_real_var (variable_name
     :        , '()', g%smi)

      else if (variable_name .eq. 'wli') then
         call respond2get_real_var (variable_name
     :        , '()', g%wli)

      else if (variable_name .eq. 'evap_plant') then
         call respond2get_real_var (variable_name
     :        , '(cm)', g%ep)

      else if (variable_name .eq. 'evap_soil') then
         call respond2get_real_var (variable_name
     :        , '(cm)', g%es)

      else if (variable_name .eq. 'evap_pot') then
         call respond2get_real_var (variable_name
     :        , '(cm)', g%eo)

      else if (variable_name .eq. 'evap_tot') then
         call respond2get_real_var (variable_name
     :        , '(cm)', g%et)

      else if (variable_name .eq. 'ep') then
         call respond2get_real_var (variable_name
     :        , '(mm)', g%ep*cm2mm)

      else if (variable_name .eq. 'ozcot_crop_in') then
         call respond2get_logical_var (variable_name
     :        , '()', g%crop_in)

      else if (variable_name .eq. 'ozcot_status') then
         call respond2get_integer_var (variable_name
     :        , '()', g%iend)

      else if (variable_name .eq. 'plant_status') then
         call respond2get_char_var (variable_name
     :        , '()', g%plant_status)

      else if (variable_name .eq. 'bolls_sc') then
         if (g%openz.gt.0.0) then
            bollsc = g%openwt/g%openz
         else
            bollsc = 0.0
         endif
         call respond2get_real_var (variable_name
     :        , '(g/boll)', bollsc)

      else if (variable_name .eq. 'nuptake') then
         call respond2get_real_var (variable_name
     :        , '(kg/ha)', g%total_n*10.0)

      else if (variable_name .eq. 'squarz_max') then
         call respond2get_real_var (variable_name
     :        , '(1/m2)', g%sqzx)

      else if (variable_name .eq. 'lai_max') then
         call respond2get_real_var (variable_name
     :        , '(m2/m2)', g%alaiz)

      else if (variable_name .eq. 'i_def') then
         call respond2get_integer_var (variable_name
     :        , '(das)', g%i_def)

      else if (variable_name .eq. 'i_def2') then
         call respond2get_integer_var (variable_name
     :        , '(das)', g%i_def2)

      else if (variable_name .eq. 'dlt_dm_green') then
         dm_crop(:) = 0.0
         dm_crop(root) = g%ddw_root
         dm_crop(meal) = 0.0  ! meal included in pod
         dm_crop(stem) = g%ddw_stem
         dm_crop(leaf) = g%ddw_leaf
         dm_crop(pod) = g%ddw_boll

         call respond2get_real_array (variable_name
     :        , '(g/m2)', dm_crop, Max_part)


      else if (variable_name .eq. 'dm_green') then
         dm_crop(:) = 0.0
         dm_crop(root) = g%dw_root
         dm_crop(meal) = g%openwt
         dm_crop(stem) = g%dw_stem
         dm_crop(leaf) = g%dw_leaf
         dm_crop(pod) = (g%dw_boll - g%openwt)
         dm_crop(pod) = l_bound(dm_crop(pod), 0.0)

         call respond2get_real_array (variable_name
     :        , '(g/m2)', dm_crop, Max_part)

      else if (variable_name .eq. 'dm_senesced') then
         dm_crop(:) = 0.0
         dm_crop(root) =  0.0
         dm_crop(meal) =  0.0
         dm_crop(stem) =  0.0
         dm_crop(leaf) =  g%leaf_res
         dm_crop(pod) =  0.0 !g%frudw_shed

         call respond2get_real_array (variable_name
     :        , '(g/m2)', dm_crop, Max_part)

      else if (variable_name .eq. 'n_senesced') then
         dm_N(:) = 0.0
         dm_N(root) =  0.0
         dm_N(meal) =  0.0
         dm_N(stem) =  0.0
         dm_N(leaf) =  g%leaf_res_n
         dm_N(pod) =  0.0 !g%frudw_shed

         call respond2get_real_array (variable_name
     :        , '(g/m2)', dm_N, Max_part)

      else if (variable_name .eq. 'n_green') then
         dm_N(:) = 0.0
         dm_N(root) =  g%n_root
         dm_N(meal) =  0.0  ! meal included in pod
         dm_N(stem) =  g%n_stem
         dm_N(leaf) =  g%n_leaf
         dm_N(pod) =   g%n_boll

         call respond2get_real_array (variable_name
     :        , '(g/m2)', dm_N, Max_part)

      else
            ! Nothing
         call message_unused ()
      endif

      call pop_routine(myname)
      return
      end subroutine



* ====================================================================
       subroutine ozcot_set_my_variable (Variable_name)
* ====================================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name to search for

*+  Purpose
*     Set one of our variables altered by some other module

*+  Changes
*      psc - 9/08/93
*      250996 jngh updated interface

*- Implementation Section ----------------------------------

*      if (variable_name .eq. '????') then
*         call collect_real_array (variable_name, '()', max_layer
*     :                               , ????, numvals
*     :                               , 0.0, 1.0)

*      else
            ! Don't know this variable name
         call Message_unused ()
*      endif


      return
      end subroutine



* ====================================================================
       subroutine ozcot_Process ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Purpose
*      Perform actions for current g%day.

*+  Changes
*      psc - 9/08/93
*      250996 jngh removed unused include

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_process')

*- Implementation Section ----------------------------------

      call push_routine(myname)
!     call patched-in ozcot model

      if (g%crop_in) then

         if(g%jdate.ne.g%isow) g%das = g%das + 1
      else
      endif

      call ozcot2 ()
      if (g%iend .ne. 0 .and. g%das .gt. 400) then
         call fatal_error (err_user
     :   , 'Crop remains unharvested at 400 DAS.'
     :   //' Check that manager harvest criteria contains'
     :   //' a test for ozcot_status > 0')
      else
      endif

      call pop_routine(myname)
      return
      end subroutine



* ====================================================================
       subroutine ozcot_Prepare ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Purpose
*     Perform calculations before the current timestep.

*+  Changes
*      psc - 9/08/93

*- Implementation Section ----------------------------------

      return
      end subroutine



* ====================================================================
       subroutine ozcot_post ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Purpose
*     Perform calculations after the current timestep.

*+  Changes
*      psc - 9/08/93

*- Implementation Section ----------------------------------

!      g%HAIL = .false.

      return
      end subroutine



* ====================================================================
       subroutine ozcot_end_run ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Purpose
*     Perform cleanup because the current simulation is about to end.

*+  Changes
*      psc - 9/08/93

*- Implementation Section ----------------------------------

      return
      end subroutine



!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!                                                                cc
!                                                                cc
!                       program ozcot                            cc
!                                                                cc
!                          27/5/83                               cc
!                                                                cc
!                                                                cc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!                                                                  c
!     begun during a visit to  t.a.e.s. blackland research center  c
!     temple, texas 1981 by a.b.hearn.                             c
!                                                                  c
!     developed at nars for namoi valley cotton  1982 to 1988      c
!     by hearn and da roza.                                        c
!         components:                                              c
!           water balance   - ritchie model taken largely from     c
!                             cornf(stapper & arkin)               c
!           nitrogen model -  developed at nars by hearn & da roza c
!           fruit submodel  - taken from siratac by hearn          c
!                                                                  c
!     ozcot1 - version for retrospective simulation of specific    c
!              crops; runs for one season only, irrigation dates   c
!              given.                                              c
!     ozcot2 - version for predictive simulation  through many     c
!              seasons; irrigation dates predicted.                c
!     ozcot3 - version for optimising irrigation                   c
!     ozcot4 - version for calibration with minos5                 c
!     ozcot5 - version for physiological analyis                   c
!                                                                  c
!                                                                  c
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!
!     structure
!       ozcot2  init
!               cinput2
!               newdate
!               metdat2 hhunc
!                       evap    fn satvp
!               decide_irg
!               solwat  sevap   fn watco
!                       swbal
!               sowday
!               emerg
!               snbal   n_fertilise
!               pltgrw  actlai
!                       laigen  fn senlf, fn stress
!                       cropn
!                       istsq
!                       fruit   bollwt  fn stress
!                               carrying_capacity fn stress
!                               overload
!                               actfru
!                               update
!                               fn frugen       fn stress
!                               fn survive
!               harvest
!               dayout2
!               (day_dudley    option for norm dudley)
!               yield
!               reset
!
!       note modules ozcot2, cinput2, metdat2, dayout2, decide_irg
!                    newdate, reset are specific to ozcot2.
!
!       ozcot.inc common blocks
!
!       input files: met.inp, soilw.inp, agron.inp - 1
!       output files: fruit.out, yield.out, calib.out - units = 2,3
!
! link/exe=ozcot2 ozcot2,init,cinput2,metdat2,hfunc,evap,satvp, -
!                 decide_irg,solwat,sevap,watco,swbal, -
!                 sowday,emerg,snbal,n_fertilise, -
!                 pltgrw,actlai,laigen,senlf,stress,cropn,istsq, -
!                 fruit,bollwt,carrying_capacity,overload,actfru, -
!                 update,frugen,survive, -
!                 harvest,dayout2,yield,reset
!
!
!                 ozcot2 - calling program
!                 ---------------------------
!
!
!      program ozcot2
* ====================================================================
      subroutine OZCOT2
* ====================================================================
      Use Infrastructure
      implicit none

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot2')

*- Implementation Section ----------------------------------
      call push_routine(myname)

!     data agron_inp/'agron.inp'/, soilw_inp/'soilw.inp'/,
!    *count_inp/'count.inp'/, lai_inp/'lai.inp'/,
!    *rain_inp/'rain.inp'/, met_inp/'met.inp'/, fcot_out/'fcot.out'/,
!    *frucal_out/'frucal.out'/, yield_out/'yield.out'/

!      data iend/0/


!psc      i=i+1

      if (g%crop_in) then

!      do 10 nszn = 1,100                     ! seasonal loop
!          do 20 i = 1,1000                   ! daily loop through whole year
!             call metdat2 (i,iend)       ! get met data
!              if(iend.eq.1)go to 31       ! end of met data
              CALL ozcot_metdat2
!             if(defirr(2).ne.0.) call decide_irg (i)  ! irrigated crop?
!              call solwat (i,dayir,npre)  ! soil water
              CALL ozcot_solwat                 ! soil water
!             if(defirr(2).ne.0.) call decide_irg       ! irrigated crop?
!             if(isow.le.0)then           ! crop sown yet?
!                  call sowday (i,iend)    ! sow tomorrow?
!                  call sowday             ! sow tomorrow?
!                  if(iend.ge.3) go to 32  ! passed sowing window or fallow
!              elseif(i.gt.isow .and. iemerg.le.0)then     ! crop emerged yet?
!jh v2001              if(g%iemrg.le.0) then
!                  call emerg (i)          ! emerge today?
!jh v2001                  CALL ozcot_emerg              ! emerge today?
!jh v2001              ENDIF
      else
      endif
!              call snbal(i)               ! soil n balance
              call ozcot_snbal                  ! soil n balance
!              if(isow.gt.0 .and. i.gt.isow) call pltgrw (i,iend,nszn)
!              if(openz.gt.0.0) call harvest(iend)
!      print*, g%crop_in, g%das, g%isow, g%openz, g%iend
      if (g%crop_in) then
              IF(g%isow.GT.0 .AND. g%das.GT.0) CALL ozcot_pltgrw
!              if(iend.eq.2)  go to 32     ! end of season
!              if(iend.ne.2)  then
!                call dayout2(i)             ! daily output
!psc                call dayout2             ! daily output
!               call day_dudley             ! output for norm dudley
!20             continue                           ! end of daily loop
!                return
!              endif
!32           continue
!              call yield(nszn,iend)           ! calculate yield
!              call reset(iend)                ! reset variables for new season
              CALL ozcot_yield                      ! calculate yield
!psc              call reset                      ! reset variables for new season
!10       continue                               ! end of seasonal loop
!31        continue
!          call exit
!      stop
         else
         endif
!        write(*,'(1x,i4, 9f5.1)')g%das, g%frucat
       call pop_routine(myname)
       return
       end subroutine




* ====================================================================
!      subroutine pltgrw (i,iend,nszn)
      subroutine ozcot_pltgrw
* ====================================================================

!-------------------------------------------------------------------
!      calls the various plant growing routines.  at this point    !
!      there are also some variable conversions to allow merging   !
!      of the independently derived soil and plant portions of the !
!      model.                                                      !
!-------------------------------------------------------------------

      Use Infrastructure
      implicit none

 !     real percent_l
!pc   integer ifrost
      integer j
      real    RTDEP2
      character string*100
      real bollgr

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_pltgrw')

!      DIMENSION PERCENT_L(10)
!      DATA PERCENT_L/0.38,0.38,0.39,0.42,0.4,5*0.0/        ! lint percent
!      data fburr /1.23/                       ! factor sc/boll to sc+burr/boll
!pc   data ifrost/0/                          ! flag for simulated frost

!----- housekeeping -----------------------------------------------------------
*- Implementation Section ----------------------------------
      call push_routine(myname)

      call ozcot_update ()

!psc      iday=i-isow ! replaced ncrpdy throughout, 15 nov 1983
      g%iday=g%das

      IF(g%iday.EQ.300) THEN
          g%iend = 10                             ! terminate crop growth
          WRITE(string,775)            ! mature bolls will be forced open.
775       FORMAT(' *** Season > 300 days; terminate crop.')
          call write_string (string)
          call pop_routine(myname)
          RETURN
      ENDIF

!---- crop development complete? ---------------------------------------------
!      IF(g%openz.GT.0.0 .AND. g%bollz.LT.1.0 .AND. g%iend.EQ.0) THEN
!jh skip row correction
      IF(g%openz.GT.0.0 .AND. g%bollz*g%rs.LT.1.0 .AND. g%iend.EQ.0)THEN
          g%iend=6                               ! bolls/m < 1; end of season
          WRITE(string,774)              ! mature bolls will be forced open.
774       FORMAT(' *** All bolls open; crop finished.')
          call write_string (string)
      ENDIF

      g%dd=g%hunits
      g%rad=g%solrad
      g%pclint = p%percent_l             ! set lint percent for variety

      IF(g%iday.gt.1) THEN
          DO 10 J=1,g%iday
              g%fyzage(J)=g%fyzage(J)+g%dd     ! physiological aging
10        continue
      ELSE
      ENDIF
      g%sumdd=g%sumdd+g%dd

!----- increase root depth ----------------------------------------------------

      g%rtdep = g%sdepth+((20.-g%sdepth)/36.)*real(g%iday)   ! G da R from 82/83 expt
      RTDEP2 = g%rtdepm*(1.-2.65*EXP(-.03*real(g%iday))) ! RTDEPM replaced 122.38 12/5/95
      IF(RTDEP2.GT.g%rtdep) g%rtdep = RTDEP2       ! added 12/5/95

!jh v2001      IF(g%iday.LE.36)g%rtdep=g%sdepth+((20.-g%sdepth)/36.)*real(g%iday) ! W*N          !const  rtdep_das_crit, rtdep_sdepth_crit
!jh v2001!cpsc        changed maximum rooting depth
!jh v2001!cpsc  if(iday.ge.37.0)rtdep=122.38*(1.-2.65*exp(-.03*iday)) ! 82/8
!jh v2001      IF(g%iday.GE.37)g%rtdep=c%rtdep_max
!jh v2001     :                       *(1.-2.65*EXP(-.03*real(g%iday))) ! 82/8                !const
      IF(g%rtdep.GT.g%rtdepm)g%rtdep=g%rtdepm


!---- check if frost terminates crop -----------------------------------------

      IF(g%tempmn.LE.c%frost_kill_immediate
     :   .and. g%iemrg .gt. 0.0) THEN    ! frost after emergence?
          IF(g%bollz.EQ.0) THEN                  ! pre-fruiting?
              g%iend=2                           ! flag for frost -
              WRITE(string,771)
              call write_string (string)
          ELSE IF(g%openz.EQ.0.0) THEN             ! green bolls yet?
              g%iend=2                           ! flag for frost - force open bolls > 80% mature
              WRITE(string,772)
              call write_string (string)
          ELSE                                 ! open bolls yet?
              g%iend=2                           ! flag for frost - force open bolls > 80% mature
              WRITE(string,773)
              call write_string (string)
          ENDIF
      ENDIF

771        FORMAT(' *** Crop killed by frost before fruiting.')
772        FORMAT(' *** Crop killed by frost during fruiting.')
773        FORMAT(' *** Crop terminated by frost.')


!---- if hail damage do here   ----------------------------------------------

!jh      IF(g%HAIL) CALL HAIL                ! hail in Karyl's expts

!----- emergence ( why not call ozcot_emerg here ------------------------------------

      IF(g%iemrg.LE.0) CALL ozcot_emerg    ! crop emerged yet? emerge today?
      IF(g%das.EQ.g%iemrg)  g%ddmerg=g%sumdd-g%dd

!----- increase leaf area -----------------------------------------------------

!      if(i.le.ilai)then
!       call actlai(i)
!       call actlai
!      else
!       call laigen(i)
        CALL ozcot_laigen
!      end if

!----- crop nitrogen ---------------------------------------------------------

!      call cropn(i)
      CALL ozcot_cropn

!---- grow plant -------------------------------------------------------------

      IF(g%isq.EQ.0) THEN
!          call istsq (i,nszn)
          CALL ozcot_istsq
      ELSE
!          if(i.gt.isq) call fruit (i,iend)
         IF(g%das.GT.g%isq) CALL ozcot_fruit
      ENDIF

      IF(g%openz.GT.0.0) CALL ozcot_harvest

      bollgr = g%bollgr
      call ozcot_dryxmatter
      call ozcot_plant_n
      g%bollgr = bollgr

!      if(isyr.eq.82 .and. jdate.eq.354) call hail(i) ! hail in 1982-83 experiment
!      if(isyr.eq.82 .and. jdate.eq.354) call hail    ! hail in 1982-83 experiment

25    continue

!------ following are for use in s/r yield -----------------------------------

      IF(g%alai.GT.g%alaiz)THEN
          g%alaiz=g%alai                        ! max LAI
          g%ilaiz=g%iday                        ! g%day of max LAI
          g%plntnz=g%plantn                     ! g%plantn on g%day of max LAI
          g%iplntn=g%ilaiz
      ENDIF

      IF(g%squarz.GT.g%sqzx) THEN
          g%sqzx = g%squarz                     ! peak square numbers
          g%isqzx = g%iday                      ! g%day of peak squares
      ENDIF

!------------------------------------------------------------------------------

      call pop_routine(myname)

      RETURN
      end subroutine


* ====================================================================
!      subroutine bollwt(idayx,l)
      subroutine ozcot_bollwt(cohort)
* ====================================================================

!     calculates increase in weight of each days' (cohort's) bolls.
!     bollgrowth rate is driven by dd,limited by water,
!     n and c(incl water effects on photosynthesis) stress

      Use Infrastructure
      implicit none


!------stuff done on 1st call of the day - stresses & growth rate -------------
      !  functions


      ! locals (i hope!)
      integer cohort
      real fbcstr
      real fbwstr
      real fbnstr
      real strsbl
      real f_temp
      real boll

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_bollwt')

*- Implementation Section ----------------------------------
      call push_routine(myname)

      IF(g%idayx.EQ.0 .OR. g%iday.EQ.g%idayx+1) THEN ! 1st call for this day?
        g%idayx = g%iday                     ! if so, set flag to stop more calls
        FBCSTR = 1.0                         ! Cc stress factor to reduce g%bollgr
        IF(g%bload.GT.0.)FBCSTR = g%carcap_c/g%bload ! supply/demand ratio for when
        IF(FBCSTR.GT.1.)FBCSTR = 1.          ! boll growth limited by Cc supply
        FBWSTR = ozcot_stress(c%fbwstr_low
     :                        ,c%fbwstr_high
     :                        ,c%fbwstr_a
     :                        ,g%smi)   ! water stress on bolls         !const  sw_stress_boll_min, sw_stress_boll_max, sw_stress_boll_pwr
!jh v2001 deleted        FBWSTR = 1.                          ! try no direct stress - 24/4/92
        IF(g%bollz+g%openz .LT. g%carcap_n) THEN ! final boll req < uptake
            FBNSTR = 1.0                     ! do not apply N stress
        ELSE                                 ! final boll req < uptake
            FBNSTR = ozcot_stress(c%fbnstr_low
     :                           ,c%fbnstr_high
     :                           ,c%fbnstr_a
     :                           ,g%fnstrs)    ! apply N stress       !const  n_stress_boll_min, n_stress_boll_max, n_stress_boll_pwr
        ENDIF
        STRSBL = AMIN1(FBCSTR,FBWSTR,FBNSTR) ! minimum of Cc, water & N stress
!        STRSBL = 1.0  ! debug

!------- efect of temperature on final boll weight ----------------------------

        IF(g%tempav.LT.20.0) THEN                                                  !const   x_temp_boll   15 20 30 35
          F_TEMP = -3.0+0.2*g%tempav          ! temperature scaling factor         !const   y_stress_boll  0  1  1  0
        ELSE IF(g%tempav.GT.30.0) THEN        ! for boll weight                    !const
          F_TEMP = 7.0-0.2*g%tempav           ! derived from Hesketh and Low 1968   !const
        ELSE                                  !
          F_TEMP = 1.0                        !  equiv to x temp 15 20 30 35                                    !const
        ENDIF                                 !           y fac   0  1  1  0
        IF(F_TEMP.LT.0.) F_TEMP = 0.
        IF(F_TEMP.GT.1.) F_TEMP = 1.

!        F_TEMP = 1.   ! debug

!jh v2001
        g%BGRVAR = p%scboll * F_TEMP * g%bper   ! nominal boll growth rate this day
        g%bollgr = g%bgrvar *1.3                ! potential unstressed rate - ex Constable            !const     bollgr_pot_fac
        g%bollgr = g%bollgr*STRSBL              ! todays actual rate per boll
!jh v2001
!jh v2001 deleted        BOLL = p%scboll*F_TEMP        ! effect of temperature
!jh v2001 deleted        g%bgrvar = BOLL*g%bper                ! boll growth rate this g%day

      ENDIF

      IF(cohort.GT.g%lfru(Flowers) .OR. g%lfru(Flowers).EQ.0) then
         call pop_routine(myname)
         RETURN
      else
      endif

!------         increase in weight of bolls (seed cotton) ---------------------------

!jh v2001 deleted      g%bollgr = g%bgrvar*STRSBL              ! todays rate per boll
!jh v2001 deleted      g%bollgr = g%bollgr *1.3                ! potential unstressed rate            !const     bollgr_pot_fac
      IF(g%bollgr.LT.0.) g%bollgr = 0.
      g%fruwt(cohort) = g%fruwt(cohort)+g%fruno(cohort)*g%bollgr ! increase today for cohort g%day bolls

!------ shed fruit not growing ------------------------------------------------

      IF(cohort.GT.g%lfru(Small_bolls)
     :   .AND. cohort.LE.g%lfru(Large_bolls)
     :   .AND. g%fruno(cohort).GT.0.) THEN
           IF(g%fruwt(cohort)/g%fruno(cohort).LT.0.1)
     :          g%frmark(cohort,age6) = g%fruno(cohort)
      ENDIF

!------------------------------------------------------------------------------
      call pop_routine(myname)
      RETURN
      end subroutine


* ====================================================================
!      subroutine carrying_capacity(i)
      subroutine ozcot_carrying_capacity
* ====================================================================

!     estimates carrying capacity of crop on basis of photosynthesis.
!     selects parameter for variety. adjusted for water stress.
!     carcap is carrying capacity, maximum number of bolls the crop
!     can carry, therefore the boll load that causes 100% shedding.

      Use Infrastructure
      implicit none


      real alight
      real radn_watts
      real p_gossym
      real pot_pn
      real pn
      real rel_p
      real templf
      real rfac
      real rm


!      data from "siratac" - 1987-88  hearn (pers. comm.)

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_carrying_capacity')
!pc   data istress/0/, ireliefco/0/

*- Implementation Section ----------------------------------
      call push_routine(myname)

!psc      if(i.eq. isq+1) then
      IF(g%das.EQ. g%isq+1) THEN
          g%istress = 0                 ! reset stress flag
          g%ireliefco = 0               ! reset stress relief counter
      ENDIF

!psc      if(bload.gt.cutout .and. smi.lt.0.75) istress = 1 ! set stress cutout flag

!psc      if(smi.gt.0.75 .and. istress.gt.0) then

      if(g%bload.gt.g%cutout.and.g%smi.lt.c%smi_affect_wlog) g%istress=1 ! 0.25 replaced 0.75 - ABH 5/11/96    !const  wlog_relief_smi
      if(g%smi.gt.c%smi_affect_wlog.and.g%istress.gt.0) then       ! 0.25 replaced 0.75 - ABH 5/11/96

          g%ireliefco = g%ireliefco + 1 ! count days since relief of stress
          IF(g%ireliefco.EQ.c%days_relief_wlog) THEN                                                     !const        wlog_relief_days
              g%istress = 0             ! end stress effect on wterlogging
              g%ireliefco = 0           ! reset counter
          ENDIF
      ENDIF
!      g%istress = 0  !debug
!----photosynthetic capacity --------------------------------------------------
!-----light interception modified to give hedgerow effect with skip row - abh 5/11/96 ------

      g%alai_row = g%alai
      IF(g%NSKIP.GT.0) g%alai_row = g%alai*g%rs         ! lai in hedgerow
      ALIGHT = (1.-EXP(-ozcot_kvalue*g%alai_row)) ! original code  - now gives interception in
                                                ! hedgerow
      IF(g%NSKIP.GT.0) THEN
         ALIGHT =ALIGHT/g%rs          ! interception on ground area basis
!jh         g%alai = g%alai/g%rs         ! restore LAI to ground area basis
      ENDIF

      RADN_watts = g%solrad*0.8942      ! convert RADN from ly to watts m**2            !const
      P_gossym = 2.391+RADN_watts*(1.374-RADN_watts*0.0005414) ! GOSSYM line1275        !const
      POT_PN = P_gossym*0.068           ! potential photosynthesis g%g/m2 CH2O          !const

      PN = POT_PN*ALIGHT*co2FertFX()    ! net photosynthesis term
!----- effect of water stress on photosysthesis -------------------------------
!jh v2001
      rel_p = 1.0
!jh v2001
      IF(g%smi .LT. c%relp_smi_crit) THEN                                                           !const    sw_stress_pn_crit
          rel_p =c%relp_intercept+c%relp_slope*g%smi              ! effect of water stress on Pp              !const    sw_stress_pn_smi_intc, sw_stress_smi_pn_slope
!          rel_p =c%relp_intercept+c%relp_slope*0.86  !debug            ! effect of water stress on Pp              !const    sw_stress_pn_smi_intc, sw_stress_smi_pn_slope
          IF(rel_p.GT.1.)rel_p = 1.           ! (TURNER g%et al 1986).                    !const
          rel_p = rel_p -c%relp_intercept                                                             !const
          rel_p = ozcot_stress(c%relp_low,c%relp_high,c%relp_a,rel_p)   ! increase severity of stress         !const   sw_stress_relp_min, sw_stress_relp_max, sw_stress_relp_pwr,
          rel_p = rel_p + c%relp_intercept                                                             !const
          PN = PN*rel_p                      ! photosynthesis adjust for water stress
      ENDIF

!----- waterlogging effect additional to n uptake - hearn & constable 1984 eqn 4

!jh v2001 deleted !c      if(istress.gt.0) then
!jh v2001 deleted !c        if(sw/ul.gt.0.87) pn = pn * 0.1 ! carrying capacity reduced
!jh v2001 deleted !c      else
!jh v2001 deleted !c        if(sw/ul.gt.0.87) pn = pn * 0.1 ! carrying capacity reduced
!jh v2001 deleted !c      endif

!---- maintenance respiration -------------------------------------------------

      TEMPLF = g%tempav+5.-10.*g%smi    ! leaf temperature                             !const
      IF(TEMPLF.LT.g%tempav) TEMPLF=g%tempav
      RFAC = 2.2**((TEMPLF-35.)/10.)    ! resp temp factor, Horie(Constable)           !const
      RM = g%sites*p%respcon*RFAC ! maintenance respiration term

!----- carrying capacity carbon - photosynthesis divided by boll growth rate

      IF(g%bgrvar.GT.0.)
     * g%carcap_c = (PN-RM)/(g%bgrvar*p%FBURR) ! carrying capacity, carbon, no stress
      IF(g%carcap_c.LT.0.) g%carcap_c = 0. ! trap
!jh v2001
!jh need rs correction
      IF(g%carcap_c.GT.500.) g%carcap_c = 500. ! limit when low temp gives low BGR
!jh v2001

!----- waterlogging effect additional to n uptake - hearn & constable 1984 eqn 4
!jh      print*, 'das, caracp_c, wli, rm, istress, bload, cutout,smi'
!jh      print*,g%das,g%carcap_c,g%wli,rm,g%istress,g%bload,g%cutout,g%smi
      IF(g%istress.GT.0) THEN
!jh v2001 deleted         IF(g%sw/g%ul.GT.c%watlog_c)
         IF(g%wli.GT.c%watlog_c)                              ! restore as DV - 30/5/96
     :      g%carcap_c = g%carcap_c  * c%wlog_carcap_red_stress ! carrying capacity reduced    !const   ! ? 0.01
      ELSE
!jh v2001 deleted         IF(g%sw/g%ul.GT.c%watlog_c)
         IF(g%wli.GT.c%watlog_c)                      ! restore as DV - 30/5/96
     :      g%carcap_c = g%carcap_c * c%wlog_carcap_red ! carrying capacity reduced    !const
      ENDIF
      g%cutout = g%carcap_c*p%fcutout ! boll load for g%cutout, sq prodn stops
!-------------------------------------------------------------------------------
!jh v2001 not used      IF(g%carcap_c .GT. 0.)THEN
!jh v2001 not used        FRUINDEX1 = 1. - g%BLOAD / g%CUTOUT
!jh v2001 not used        FRUINDEX2 = 1. - g%BLOAD / g%carcap_c
!jh v2001 not used      ELSE
!jh v2001 not used        FRUINDEX1  = -1.0
!jh v2001 not used        FRUINDEX2  = -1.0
!jh v2001 not used      END IF

      call pop_routine(myname)
      RETURN
      end subroutine


* ====================================================================
!      subroutine cropn(i)
      subroutine ozcot_cropn
* ====================================================================

      Use Infrastructure
      implicit none

      real harvest_n
      real bgr
      real UP_KG_HA
      real VEG_N

!     assumes all n available for season (availn) from snbal
!     is taken up (uptakn).
!     computes: n harvested on basis of constable and rochester 1988
!               n in fruit frun
!               carrying capacity (carcap_n), number of bolls for which
!                       harvest_n is adequate
!                       used in frugen and survive for squares
!               stresses:
!                       vegetative (vnstrs for laigen) = f(1-harvest_n/uptakn)
!                               ie function of proportion of n remaining in veg
!                         fruit (fnstrs for bollwt) 1- frun/harvest_n
!                               ie function of n to be harvested not in fruit


      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_cropn')

*- Implementation Section ----------------------------------
      call push_routine(myname)

!----- potential uptake for season --------------------------------------------
!jh v2001 changed      g%uptakn = g%availn   ! potential uptake for season based on available N
      g%uptakn = g%availn / 10.0   ! potential uptake for season based on available N

!-----  compute potential n harvested ----------------------------------------

      HARVEST_N = g%uptakn * c%harvest_n_frac    ! harvestable N; 0.85 fromConstable & Rochester       !const
!jh v2001 deleted      HARVEST_N = HARVEST_N/10.      !  kg/ha to g%g/m**2, N limiting fruit.               !const

!----- compute n already in fruit -------------------------------------------
!pdev this is generating an underflow warning:
!pdev ---------------------------------vvvvvvvvvv
!jh v2001
      UP_KG_HA = g%uptakn*10        ! need kg/ha for next calculation
      g%seed_nc = .02407+0.000147*UP_KG_HA-0.00000034*UP_KG_HA**2 ! GAC dat 3/6/88         !const
!jh v2001
      g%frun = (g%frudw+g%openwt)
     :       * ((1.-g%pclint)*g%seed_nc + (p%FBURR-1.)* 0.005) ! N in frt                                                                     !const

!----- compute n carrying capacity --------------------------------------------

      BGR = p%scboll                ! seed coton per boll
      g%carcap_n = HARVEST_N /(BGR*0.6*0.03) ! bolls per m                                 !const

!----- compute n stresses -----------------------------------------------------

!jh v2001 deleted      g%vnstrs = (g%uptakn-10.)/g%uptakn ! vegetative stress 10=uptake @ 1st boll          !const
!jh v2001

      IF (g%bollz .EQ. 0.0) THEN        ! during veg growth, before 1st boll
          VEG_N = g%uptakn              ! N in veg tissues before 1st boll
      ELSE                              ! during reproductive growth
          VEG_N = g%uptakn - g%frun     ! N in veg tissues after 1st boll
      ENDIF

      IF(VEG_N .GT. 0.0) THEN
          g%vnstrs = (VEG_N - 1.0) / VEG_N  ! vegetative stress, 10=uptake @ 1st boll
      ELSE
          g%vnstrs = 0.0
      ENDIF
!jh v2001
      IF(g%bollz.EQ.0.0) THEN            ! during veg growth, before 1st boll
          g%fnstrs = 1.0
      ELSE
!jh v2001
        IF(HARVEST_N .GE. g%frun) THEN
          g%fnstrs = 1.0 - g%frun/HARVEST_N ! fraction of harvestable not in fruit
        ELSE
          g%fnstrs = 0.0
        ENDIF
!jh v2001
      ENDIF

      IF(g%vnstrs.GT.1.0) g%vnstrs=1.0
      IF(g%vnstrs.LT.0.0) g%vnstrs=0.0
      IF(g%fnstrs.GT.1.0) g%fnstrs=1.0
      IF(g%fnstrs.LT.0.0) g%fnstrs=0.0
!----------------------------------------------------------------------------

      g%uptakn = g%uptakn*10.              ! g/m2 to kg/ha

      call pop_routine(myname)
      RETURN
      end subroutine


* ====================================================================
!      subroutine emerg (i)
      subroutine ozcot_emerg
* ====================================================================
!------------------------------------------------------------------------------------------------
!
!     Simulates  emergence
!
!     Originally Wanjura's complex function was used.
!     Replaced by a simple heat sum (60 DD) as an alternative, probably in 1990.
!     Revised in Nov 1996 by ABH at APSRU in order to deal with adverse conditions.
!     Data from Constable 1976, Anderson 1971 and Wanjura et al 1969 and 1970 were used.
!     Wanjura's 1970 complex function was linearised to give 0.0041cm/hr/deg which gives
!     0.1 cm per DD12, which when divided into seed depth (SDEPTH) gives DD requirement = 50
!     for sowing at 5 cm.  5DD in soil is equivalent to  6DD air in Sept/Oct,
!     to give 60 DD for emergence, as previously used.
!     Anderson 's data gave values of DD requirement from 50 to 75,
!     the variation being associated with Tmax.
!     Consequently the DD requirement increases by 2.5 each deg C of mean Tmax over 20 deg C.
!     All three papers, and others, show a relationship between days to emerge and % emergence.
!     Constable's data allowed a quantitative relationship to be deleveloped.
!     Constable's data also showed an effect of soil water; when tensiommeter readings were zero,
!     (ie soil saturated SMI>0.87) emergence was reduced by 20%, consequently emergence
!     was reduced by 0.03 per daywhen SMI>0.87.
!     Wanjura 1969 contained data relating subsequent survival of seedlings to elapsed
!     days to emergence.
!
!     Constable's original data will be reworked to derive relationships directly,
!     instead of indirectly.
!
!------------------------------------------------------------------------------------------------

      Use Infrastructure
      implicit none

      real    ESTABLISH
      character string*100

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_emerg')

*- Implementation Section ----------------------------------
      call push_routine(myname)

      IF(g%iemrg.GT.0) then
         call pop_routine(myname)
         return
!jh v2001 deleted      else
!jh v2001 deleted !c----- simple heat sum as an alternative to wanjura's function -----

!jh v2001 deleted         IF(g%sumdd .LT. 60.) then                                            !const
!jh v2001 deleted            !RETURN
!jh v2001 deleted         else
!jh v2001 deleted            g%iemrg = g%das
!jh v2001 deleted         endif
      endif

!--------------------------------------- ------------------------------------------------------


      IF(g%INITIAL.EQ.1) THEN               ! first call of season
          g%INITIAL = 0                       ! reset flag for next season
          g%NDAY_CO = 0                       ! counter for days to emergence
          g%NWET_CO = 0                       ! counter for wet days to mergence
          g%SUM_TMX = 0.0
          g%AVE_TX  = 0.0
          g%DELAY_emerg =  0.0
          g%DD_EMERG = (g%SDEPTH/p%rate_emergence)*(6.0/5.0) ! 0.1 is from linear increase phase of Wanjura 1970
          g%PPM_SOWN =  0.0
          g%PPM_EMERGE = 0.0
          g%PPM_ESTABLISH =  0.0
          g%FAIL_EMRG = 0.0
          g%F_DIE =  0.0
      ENDIF

!--------------------------------------- ------------------------------------------------------
      g%NDAY_CO = g%NDAY_CO + 1                       ! count days to emergence
      IF(g%SMI .GT. 0.87) g%NWET_CO = g%NWET_CO + 1   ! count wet days to emergence
      g%SUM_TMX = g%SUM_TMX + g%TEMPMX                ! sum TEMPMX
      g%AVE_TX = g%SUM_TMX/g%NDAY_CO                    ! mean Tmax during mergence
      IF(g%AVE_TX .GT. 20.0) g%DELAY_emerg = (g%AVE_TX - 20.0)*2.5  ! increase DD requirement - Anderson's data

      IF(g%SUMDD .LT. g%DD_EMERG+g%DELAY_emerg) then
         call pop_routine(myname)
         RETURN   ! does not emerge today
      endif

!------ Crop emerges ---------------------------------------------------------------------------

      g%iemrg = g%das

!      IF(MODE.LE.1) THEN
!         PPM = PPM_TARGET                       ! input gives established population
!          WRITE(2,146)  JDATE,IDAY,PPM
!  146     FORMAT(I4,I6,' *** Crop emerged with',F5.1,
!     *                 ' plants per m sq.')
!jh      IF(MODE.LE.2) THEN                         ! MPB setting population in management mode
!jh          PPM = PPM_TARGET                       ! input gives established population
!jh          WRITE(2,146)  JDATE,IDAY,PPM
!jh 146     FORMAT(I4,I6,' *** Crop emerged with',F5.1,
!jh     *                 ' plants per m sq.')
!jh      ELSE                                       ! MODE=2, simulate population

!------ Estimate emergence ---------------------------------------------------------------------

          g%FAIL_EMRG = 0.015*real(g%NDAY_CO-7)            ! slow emergence; Constable 1976
          g%FAIL_EMRG = g%FAIL_EMRG + 0.03*real(g%NWET_CO)   ! wet conditions; Constable 1976
          IF(g%FAIL_EMRG .GT. 1.0) g%FAIL_EMRG = 1.0
!jh v2001
          IF(g%FAIL_EMRG .LT. 0.0) g%FAIL_EMRG = 0.0
          g%PPM_SOWN = g%PPM_TARGET/0.8               ! seeds sown - assume 80% emerge
          g%PPM_EMERGE = g%PPM_SOWN*(1.0 - g%FAIL_EMRG)   ! adjust PPM for failure to emerge

!------ Estimate establishment   -----------------------------------------------------------

          IF(g%NDAY_CO .GT. 8) THEN                  ! estimate proportion of seedlings
              g%F_DIE = 0.0567*3.0 + 0.1*(g%NDAY_CO - 8) ! that fail to survive as a function
          ELSEIF(g%NDAY_CO .GT. 5) THEN              ! of days to emerge
              g%F_DIE = 0.0567*(g%NDAY_CO - 5)
          ENDIF
          IF(g%F_DIE .GT. 1.0) g%F_DIE = 1.0

          g%PPM_ESTABLISH = g%PPM_EMERGE*(1.0-g%F_DIE)  ! adjust PPM for seedling death
          g%PPM = MIN(g%PPM_TARGET,g%PPM_ESTABLISH)    ! to avoid confusion, PPM cannot exceed target

!jhtemp disable establishment
          g%PPM = g%PPM_TARGET

          ESTABLISH = g%PPM*100.0/g%PPM_TARGET         ! percentage established


              WRITE(string,147)  g%PPM,ESTABLISH, g%PPM_TARGET
  147         FORMAT(' *** Crop emerged with',F5.1,
     :            ' plants per m sq,',F4.0,'% of target population of '
     :            , f5.1)
               call write_string (string)

          IF(ESTABLISH.LT.25.)THEN
              g%IEND = 5           ! flag to terminate season
              WRITE(string,148) g%PPM_TARGET
  148         FORMAT(' *** Crop failed to establish;',
     :             ' stand was less than 25% of target population of '
     :             , f5.1)
              call write_string (string)
          ENDIF
!jh      ENDIF                                      ! end of IF BLOCK for modes

      call pop_routine(myname)
      RETURN
      end subroutine

!-------------------------------------------------------------------

!      previous version jackson/arkin

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!      calculates hypocotle elongation leading up to emergence     c
!      based on the growing-degree-day concept.                    c
!      the elongation rate of 0.12 cm/gdd is use to a maximum rate c
!      of 2.04 cm/gdd at about 32 c.                               c
!      the gdd base temperature is set in the "init" subroutine.   c
!      elongation rate data are from wanjura et.al. 1970.          c
!                                                                  c
!      key variables:                                              c
!          sdepth = seed sowing depth                              c
!          stemp = soil temperature (calc. in subroutine "metdat") c
!          iemrg = emergence date                                  c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!       t = stemp                             ! soil temp
!       if(stemp.lt.14.44)t = 14.44+(14.44-t) ! to comput lag whenstemp < 14.44
!       hypoel = 0.0853-0.0057/(41.9-t)*(t-34.44)**2 ! wanjura's function
!       hypoel = hypoel*24.                   ! convert to daily rate
!       hypoel = hypoel*0.6                   ! tune to namoi valley
!       if(stemp.lt.14.44)hypoel = hypoel*(-0.5) ! delay below 14.44
!       esum = esum+hypoel

!      if(esum .lt. sdepth) return
!      iemrg = i

!      return
!      end subroutine


* ====================================================================
!      subroutine evap (i)
      subroutine ozcot_evap
* ====================================================================
! retained to calculate EO for EP calculation.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!      calculates potential evapotransperation from modified pen-  c
!      man equation as reported by ritchie.                        c
!                                                                  c
!      key variables:                                              c
!          delta = slope of the water vapor/ air temp. curve       c
!          albedo = combined plant-soil albedo                     c
!          h = net radiation balance                               c
!          eo = potential et over bare soil                        c
!          eos = potential et below plant canopy                   c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      Use Infrastructure
      implicit none


      real elev
      real Pp
      real gamma
      real tk
      real delta
      real d
      real xn1
      real svpmax
      real svpdry
      real svpwet
      real vpdry
      real salpha
      real albedo
      real r4
      real r6
      real h
      real at
      real rns
      real F_INT

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_evap')

*- Implementation Section ----------------------------------
      call push_routine(myname)

!---------- calculate bowen ratio term -----------------------------------------

      ELEV=c%elevation_default                                    ! ELEVATION (M)                  !const
!jh        ELEV=200.0                                    ! ELEVATION (M)                  !const
      Pp=1013.0-0.105*ELEV                          ! MEAN PRESSURE(MB)              !const
      GAMMA=6.6E-04*Pp*(1.0+0.00115*g%tempwt)    ! PSYCHROMETER CONSTANT             !const
      TK=g%tempav+273.                                ! MEAN TEMP(DEG KELVIN)        !const
      DELTA=(EXP(21.255-5304./TK))*(5304./(TK**2.)) !SLOPE g%sat VAP PRES CURVE      !const
      D=DELTA/(DELTA+GAMMA)

!------ calculate fraction radiation reaching surface(tr) ----------------------
!       culls functions(unpublished)


      XN1=0.404*ALOG(g%s)+1.49                                                      !const
!psc
      g%alai_row = g%alai
      IF(g%NSKIP.GT.0)g%alai_row = g%alai*g%rs        ! lai in hedgerow
      IF(g%alai_row.LT.XN1) THEN     ! when LAI below XN1 threshold
         g%tr=EXP(-0.6*g%alai_row)                                                    !const
      ELSE                        ! when LAI above XN1 threshold
         g%tr=EXP(-0.398*g%alai_row)                                                  !const
      ENDIF

!psc
      IF(g%NSKIP.GT.0) THEN
         F_INT = 1.0-g%tr              ! intercetion in hedgerow
         F_INT = F_INT/g%rs          ! interception on ground area basis
         g%tr =  1.-F_INT            ! transmission on ground area basis
!jh         g%alai = g%alai/g%rs        ! restore LAI to ground area basis
      else
         F_INT = 0.0
      ENDIF

      g%f_intz = max(g%f_intz, f_int)

!      if(rs.gt.1.) tr = (tr+rs-1.)/rs ! adjust for rows wider than im

!-----vapor pressure deficit: mean of vpd at 9am and 3pm ,assuming atmos.
!     vp.(vpdry) remains constant throughout day, and tmax=t3pm-----------------

      SVPMAX=ozcot_satvp(g%tempmx)      ! g%sat VP AT TMAX=g%sat VP AT T3PM
      SVPDRY=ozcot_satvp(g%tempdy)      ! g%sat VP AT 9AM TDRY
      SVPWET=ozcot_satvp(g%tempwt)      ! g%sat VP AT 9AM TWET

      VPDRY=SVPWET-GAMMA*(g%tempdy-g%tempwt) ! atmospheric VP
      IF(VPDRY.LE.0.) VPDRY = 0.1         ! cannot be -ve.
      g%vpd=((SVPDRY-VPDRY)+(SVPMAX-VPDRY))/2.

!------ calculate potential evaporation (eo) -----------------------------------

      SALPHA=0.09                                                                  !const
      ALBEDO=0.3067+0.3333*SALPHA-((0.23-SALPHA)/0.75)*g%tr ! ??                   !const

!------net longwave radiation(r6,cal/cm2)--------------------------------------

!      ritchie(1975)unpub.;idso & jackson(1969)j geophys res,74:5397-5403;
!      jensen et al(1969)proc am soc civil engin,96:61-79                             ! temple,texas
!        r4=1.-.261*exp(-7.77e-04*tempav**2.)        ! sky emissivity,ea
!        r6=(r4-.96)*1.17e-07*tk**4.*(.2+.8*solrto)  ! ((ea-es)*@*tk**4)*(-)

      R4=1.08*(1.-EXP(-VPDRY**(TK/2016.)))                                           !const
      R6=(R4-1.)*1.*1.17E-07*TK**4.*(.2+.8*g%solrto) ! ((EA-g%es)*@*TK**4)*(-)       !const
                                                     ! g%es=EMISSIVITY OF EVAP SURFACE  !const

!-------net radiation(h,cal/cm2)----------------------------------------------

      H=(1.-ALBEDO )*g%solrad+R6
      IF(H.LT.0.0) H=0.0

      g%ho=H/583.                                 ! NET RADIATION(CM)             !const
!       go=g/583.                                   ! soil heat flux(cm)

!-------advection or aerodynamic term ------------------------------------------

      IF(g%wind.NE.0..AND.g%tempwt.NE.0.) THEN
         AT = (1.-D)*0.01*(1.+.028*g%wind)*g%vpd ! advection: g%wind & g%vpd       !const
      ELSE IF(g%wind.EQ.0..AND.g%tempwt.NE.0.) THEN
         AT = -11.9813+0.012*g%vpd+0.0404*TK     ! advection: g%vpd but no g%wind   !const
      ELSE
         AT = -20.4355+0.0697*TK                 ! advection:  no g%vpd or g%wind   !const
      ENDIF
      IF(AT.LT.0.0) AT = 0.0

!jhnote test removing this
      g%eo=g%ho*D+AT                              ! GdaR Mar'85, update ABH Mar88.

!------ calculate potential below canopy evaporation (eos) ---------------------

!jh v2001        RNS=g%ho*g%tr ! GdaR Mar'85
!jh v2001        g%eos=D*RNS ! GdaR Mar'85
!jh v2001        IF(g%eos.LT.0.0)g%eos=0.0
!jh v2001        IF(g%eos.GT.g%eo)g%eos=g%eo

        call pop_routine(myname)
        RETURN
        end subroutine


* ====================================================================
!      function frugen(i)
      real FUNCTION ozcot_frugen(ndas)
* ====================================================================
!
!     estimates generation of new fruit as function of existing
!     fruiting sites and bolload adjusted for nitrogen stress

!     "Cutout" occurs in FRUGEN when squaring (fruiting site) production stops.
!     which is caused by:
!           boll N demand > supply  --  accounted for by FNSTRS=0, terminal
!           boll C demand > supply  --  which occurs when
!               (i) assimilation not reduced by water stress or waterlogging
!                   and boll load exceeds capacity; terminal until 2nd cycle
!                   cycle after enough bolls open to reduce load, but capacity
!                   may be reduced by this time by reduced LAI
!               (ii) assimilation reduced by water stress or waterloggin;
!                   this need not be terminal if the stress ends, but the rate
!                   of squaring will be rediced if the stress is prolonged,

!      data from "siratac" - 1987-88 hearn (pers. comm.).

      Use Infrastructure
      implicit none

      integer ndas



!pc   real sites1
      real blr
      real dfru
      real vsnstr
      real ppm_row
      real popfac

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_frugen')

*- Implementation Section ----------------------------------
      call push_routine(myname)

!-----------------------------------------------------------------------------
!     initialise on first call for new crop
!-----------------------------------------------------------------------------

!      if(i.eq.isq+1)then           ! 1st call,ie day after 1st square
      IF(ndas.EQ.g%isq+1)THEN         ! 1st call,ie g%day after 1st square
          g%n_cutout=0             ! flag to show g%cutout, >= 1 = not squaring
          g%sites1=0.0             ! to save current value of g%sites
      ENDIF
      ozcot_frugen=0.                    ! delete y'day's value and reset to zero
!-----------------------------------------------------------------------------
!     is this 1st cycle, cutout or 2nd cycle (regrowth)?
!     crop cuts out, either temporarily or terminaly, when boll load exceeds
!     the value CUTOUT (from S/R CARRYING). CUTOUT is zero the day after 1st
!     square, which is therefore treated as a special case below.
!-----------------------------------------------------------------------------

      IF(g%cutout.GT.0. .and. g%bollz.gt.0.0) THEN        ! post 1st flower
          BLR=g%bload/g%cutout     ! boll load ratio
          IF(BLR.GT.1.) BLR=1.     ! full boll load
      ELSE IF(g%cutout.GT.0. .and. g%bollz.eq.0.0) THEN  ! pre-1st fl / all boll shed
!          IF(ndas.EQ.g%isq+1) THEN      ! day after 1st square ?
              BLR = 0.             ! if so, no boll load
      ELSE IF(g%cutout.eq.0. .and. ndas.EQ.g%isq+1) THEN
          BLR = 0.             ! if so, no boll load
      else                     ! CUTOUT=0 and not day after 1st square
              BLR = 1.             ! full boll load because CUTOUT=0
          ENDIF
!      ENDIF

!     if cutout is caused by water stress (ie SMI < 0.25) then
!     10% of fruiting sites become inactive for FRUGEN every day after 5 days
!     which simulates lag in recovery after relief of stress
!-----------------------------------------------------------------------------
      IF (BLR.EQ.1.0 .OR. g%FNSTRS .EQ. 0.0) THEN
          g%n_cutout = g%n_cutout + 1         ! count days cutout
          if(g%n_cutout.GT.c%cutout_smi_days)
     :                 g%sites1 = g%sites1
     :                          + c%cutout_smi_site_red*g%size*g%ppm ! inactive g%sites          !const

         call pop_routine(myname)
         RETURN

      ELSE IF(g%n_cutout.GE.1)THEN          ! stress finished, squaring resumes
          IF(g%sites1.GT.g%sites) g%sites1 = g%sites
          g%n_cutout = 0                      ! recovery complete, squaring resumes
      ENDIF

!-----------------------------------------------------------------------------
!     square production for this day
!-----------------------------------------------------------------------------

      g%size = (g%sites-g%sites1)/g%ppm ! active g%sites per plant for FRUGEN & SURVIV
      IF(g%size.LT.1.0) g%size = 1.0 ! average  plants has 1 site
      IF(g%size.LT.0.5/g%ppm) g%size = 0.5/g%ppm ! average  plants has 1 site              !const

      DFRU = p%SQCON*SQRT(g%size)*g%ppm*(1.-BLR)   ! sites per DD
      VSNSTR = ozcot_stress(c%vsnstr_low           ! vegetative N stress
     :                     ,c%vsnstr_high
     :                     ,c%vsnstr_a
     :                     ,g%vnstrs)                              !const
      DFRU = DFRU * VSNSTR

      PPM_ROW = g%ppm*g%rs               ! plants per m row for POPFAC
      POPFAC = 1./(1.+p%POPCON*PPM_ROW)   ! plant population factor within row
      DFRU = DFRU * POPFAC               ! adjust for plant population
      ozcot_frugen = DFRU * g%dd               ! today's squares
      IF(ozcot_frugen.LT.0.0) ozcot_frugen=0.0

      call pop_routine(myname)
      RETURN
      end function


* ====================================================================
!     subroutine fruit (i,iend)
       subroutine ozcot_fruit
* ====================================================================

!      ages fruit, sums fruit in categories, estimates physiological
!      shedding and survival of fruit.  calls s/r actfru to estimate
!      new squares flowers and open bolls from counts. alternatively
!      calls s/r ozcot_frugen to estimate new squares when counts not
!      available.

      Use Infrastructure
      implicit none


!jh      real frudd
!jh      real wt
      character string*100
      integer cat
      integer age
      integer ndayfl
!jh      real bltme
      real surv
      integer cohort
      integer lf
      integer mm
      integer mmm
      integer nfl
      integer if
      real    fruit_wt_shed

!      dimension frudd(8),wt(8),sfmcat(8),bltme(8),bpsum(300)
!jh      DIMENSION FRUDD(8),WT(8),SFMCAT(8),BLTME(8)
!jh      DATA FRUDD/50.,180.,350.,380.,520.,660.,870.,1100./
!jh      DATA BLTME/3*0.0,0.07,0.21,0.33,0.55,1.0/
!jh      DATA WT/.0104,.0272,.1441,.0988,.5042,.9617,1.0,.5785/

!      data scboll /5.0,5.0,4.7,4.5,5.5,4*.0,7./  ! dp16,dp61,dp90,siok,sica
!      data respcon/.025, .025, .02306, .01593, .02306, 4*.0,.025/ !  ditto
!      data sqcon  /.021, .021, .02057, .02283, .02057, 4*.0,.021/ !   ditto
!      data fcutout/.4789, .4789, .4789, .5411, .4789,  4*.0,.48/ !  ditto
!      data flai   /1.0, 1.0, 0.87, 0.52, 0.87, 4*0.0,1./         !  ditto
!      data popcon /.03633/

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_fruit')

*- Implementation Section ----------------------------------
      call push_routine(myname)

!----  re-intialise arrays and variables to zero where appropriate -------------

!      if(i.eq.isq+1)then        ! 1st call of new season on day after 1st square
!          do 10 n=1,300
!              bpsum(n)=0.0
!10        continue
!          idayx = 0             ! flag to bollwt, called for 1st time this day
!      endif

      DO 100 cat = 1,Max_categories-1
          g%frucat(cat) = 0.0
          g%sfmcat(cat) = 0.0
          IF(cat.GT.7)GO TO 100
          DO 20 age=1,7
              g%fmkcat(cat,age)=0.
20        continue
100   continue

      g%frudw = 0.
      NDAYFL=0

!---- compute boll period ------------------------------------------------------

	   g%bper = EXP(5.385-.0512*g%tempav)         ! boll period Constable
      g%bper = g%bper*(p%FRUDD(Inedible_bolls)-p%FRUDD(Large_Sqz))/788.   ! adjust for variety
      g%bper = 1.0/g%bper                        ! fraction for day

!-----------------------------------------------------------------------------
!     the next loop ( do 200 l=....) goes through the fruit arrays
!     starting with the element with oldest fruit and therefore the smallest
!     index, in order to develop or abscise fruit, and put in categories.
!     before entering the loop, find the oldest fruit.
!-----------------------------------------------------------------------------

      LF=g%lfru(open_bolls)    ! 1st active element in fruit array, oldest immature fruit
!psc      if(lf.eq.0)lf=isq-isow-10 ! start in fruno 10 days before 1st square
      IF(LF.EQ.0)LF=g%isq-10 ! start in g%fruno 10 days before 1st square             !const

!psc      do 200 l=lf,i-isow-1 ! loop from oldest growing boll to previous day
      DO 200 cohort=LF,g%das-1 ! loop from oldest growing boll to previous g%day
          IF(cohort.GT.Max_cohort)GO TO 200
!psc          call bollwt(idayx,l)
          CALL ozcot_bollwt(cohort)

!---- age & abscise marked fruit----------------------------------------

          DO 30 age = 1,max_age-1
              MM = (max_age-1)-age+1
              MMM = MM+1
              g%frmark (cohort,MMM) = g%frmark(cohort,MM)
30        continue
          g%frmark(cohort,age1)=0.                 ! CLEAR FIRST g%day
          IF(g%fruno(cohort).GT.0.
     :      .AND. g%frmark(cohort,age7).GT.0.)THEN ! fruit shed today?
              IF(g%frmark(cohort,age7).GT.g%fruno(cohort))THEN
                  write(string,999) cohort,g%fruno(cohort)
     :                                    ,g%frmark(cohort,age7)
999               format
     :            (' too many marked fruit:',i4,2f5.1)
                  call write_string (string)
                  g%frmark(cohort,age7) = g%fruno(cohort)
              ENDIF

              fruit_wt_shed = g%fruwt(cohort)
     :                      * g%frmark(cohort,age7)
     :                     / g%fruno(cohort) ! adjust wt sheds
              g%frudw_shed = g%frudw_shed + fruit_wt_shed
              g%fruwt(cohort) = g%fruwt(cohort) - fruit_wt_shed ! adjust wt sheds
              g%fruno (cohort)=g%fruno(cohort)-g%frmark(cohort,age7) ! remove marked fruit
          ENDIF

!---- sort fruit and marked fruit into age categories  -----------------------

          IF(g%fyzage(cohort).GT.p%FRUDD(Large_Sqz))
     *    g%bpsum(cohort)=g%bpsum(cohort)+g%bper          ! develop g%day cohort's bolls
          IF(g%n_def.GT.0) g%bpsum(cohort)=g%bpsum(cohort)/0.99 ! develop faster after defoliation      !const

          IF(g%iend.EQ.2 .OR. g%iend.EQ.6) THEN     ! frost or green bolls > 1
              IF(g%bpsum(cohort).GE.0.9) g%bpsum(cohort)=1.0  ! crop finishing; opens phys mat bolls
!              IF(g%bpsum(cohort).GE.0.98) g%bpsum(cohort)=1.0  ! crop finishing; opens phys mat bolls
          ENDIF

          ! determine which fruit age category this cohort is in
          DO 40 cat=1,Max_categories-1                              ! stay in loop until category found
              IF(cat.LT.Flowers)THEN                    ! if yes, squares
                  IF(g%fyzage(cohort).LT.p%FRUDD(cat)) GO TO 204
              ELSE                              ! no, therefore flowers or bolls
                  IF(g%bpsum(cohort).LT.p%BLTME(cat))GO TO 204
              ENDIF
40        continue

!------- if last loop completed, fruit are open bolls -------------------------

          IF(cohort.LE.g%lfru(open_bolls))GO TO 200 ! this days bolls already open?
          g%lfru(open_bolls)=cohort               ! this days bolls open today, reset marker
!          if(i.le.idate)go to 202   ! use actual counts or not?
          g%openz=g%openz+g%fruno(cohort) ! simulated bolls open added to total
          g%openwt=g%openwt+g%fruwt(cohort)
          g%frucat(cat)=g%frucat(cat)+g%fruno(cohort)       ! sum fruit nos in categories

!202       continue

          g%fruno(cohort)=0.0 ! delete open bolls from array
          g%fruwt(cohort)=0.
          GO TO 200

204       continue
          IF(cohort.LE.g%lfru(cat))GO TO 206 ! this days fruit in category cat  yet?
          g%lfru(cat)=cohort               ! this days fruit into category cat today.
          IF(cat.NE.Flowers)GO TO 206       ! category is flowers?
          NDAYFL=NDAYFL+1           ! count this days flowering
!          if(i.gt.idate)go to 206   ! using actual counts or not?
!          fruno(l)=0.0              ! clear for actual counts
!          go to 200

206       continue
          g%frucat(cat)=g%frucat(cat)+g%fruno(cohort)       ! sum fruit nos in categories
          IF(cat.GE.Flowers) g%frudw = g%frudw+g%fruwt(cohort)  ! sum dry wt of fruit
          IF (cat.GT.Large_bolls) GO TO 200
          DO 50 age=2,6                                 ! loop thro marked fruit
              g%fmkcat(cat,age)=g%fmkcat(cat,age)+g%frmark(cohort,age) ! sum marked fruit
              g%sfmcat(cat)=g%sfmcat(cat)+g%frmark(cohort,age)    ! sum marked fruit for g%bload
50        continue

200   continue

!---- total squares, green (growing) bolls, open bolls---------------------

      g%squarz=0.
      g%bollz=0.
      g%bload=0.                                     ! reset
      DO 60 cat=1,Max_categories-1
          IF (cat.LE.Large_Sqz) g%squarz=g%squarz+g%frucat(cat)  ! total squares
          IF (cat.GE.Flowers.AND.cat.LE.Inedible_bolls)
     :                                 g%bollz=g%bollz +g%frucat(cat) ! total bolls
          g%bload=g%bload+(g%frucat(cat)-g%sfmcat(cat))*p%WT(cat)  !  boll load
60    continue

!---- this day's production of fruit -----------------------------------------

!      call carrying_capacity(i)
      CALL ozcot_carrying_capacity
      IF(g%bload.GT.g%carcap_c .OR. g%bollz+g%openz.GT.g%carcap_n)
     :   CALL ozcot_overload                                  ! abort excess fruit

!      if (i.le.idate)then                            ! use counted fruit?
!      if (das.le.idate)then                            ! use counted fruit?
!          call actfru (i)
!          call actfru
!          call update(1,1,daysqz,squarz,frudd)
!          if(lfru(4).ne.0..or.daysfl.ne.0.)
!     *        call update(4,ndayfl,daysfl,bollz,frudd)
!          if(lfru(9).ne.0..or.daysop.ne.0.)
!     *        call update(9,1,daysop,openz,frudd)
!      else
!psc           icount = i               ! dummy parameter to use frugen
!          fruno(i-isow)=frugen(i)  ! frugen is function generating squares
          g%fruno(g%das)=ozcot_frugen(g%das) ! ozcot_frugen is function generating squares
!      end if

!---- mark this day's fruit for physiological shedding      -----------------

      SURV = ozcot_survive(g%carcap_c,g%bload) ! square survival rate

!      if(jdate.lt.59) surv = 0.1       ! for krs with delayed protection
!      surv =surv*0.33                  ! for pest damage in 1972/3 namoi

!      frmark(i-isow,1)=fruno(i-isow)*(1.-surv)
!      if(frmark(i-isow,1).lt.0.) frmark(i-isow,1)=0.0
!      fmkcat(1,1)=fmkcat(1,1)+frmark(i-isow,1)
      g%frmark(g%das,age1)=g%fruno(g%das)*(1.-SURV)
      IF(g%frmark(g%das,age1).LT.0.) g%frmark(g%das,age1)=0.0
      g%fmkcat(Small_Sqz,age1)= g%fmkcat(Small_Sqz,age1)
     :                        + g%frmark(g%das,age1)
      IF(NDAYFL.EQ.0)GO TO 501
      SURV = ozcot_survive(g%carcap_c,g%bload) ! boll survival rate
!      surv =surv*0.33                  ! for pest damage in 1972/3 namoi

      DO 70 NFL = 1,NDAYFL
          IF = g%lfru(Flowers)-NFL+1
          g%frmark(IF,age1) = g%fruno(IF)*(1.-SURV)
          IF(g%frmark(IF,age1).LT.0.)g%frmark(IF,age1) = 0.0
          g%fmkcat(Flowers,age1) = g%fmkcat(Flowers,age1)
     :                          + g%frmark(IF,age1)
70    continue

!---- add new fruit to totals ------------------------------------------------

  501 CONTINUE

!      sites = sites+fruno(i-isow)
!      if(i.le.idate)return ! squarz & frucat updated in update
!      squarz = squarz+fruno(i-isow)
!      frucat(1) = frucat(1)+fruno(i-isow)
      g%sites = g%sites+g%fruno(g%das)
!jh      IF(g%das.LE.g%idate) then
         !RETURN ! g%squarz & g%frucat updated in UPDATE
!jh      else
         g%squarz = g%squarz+g%fruno(g%das)
         g%frucat(Small_Sqz) = g%frucat(Small_Sqz)+g%fruno(g%das)
!jh      endif
!-----------------------------------------------------------------------------
      call pop_routine(myname)
      RETURN
      end subroutine


* ====================================================================
!      subroutine harvest(iend)
      subroutine ozcot_harvest
* ====================================================================

!     this subroutine simulates defoliation and picking
!     use n_def, n_pick and j_pick for cost of defoliation:       nb action
!         cost of defoliant is n_def * $25                 <-- for gross margins
!         cost of picking is n_pick * $?                   <-- for gross margins
!         cost of delay is (j_pick - 91) * $ per day       <-- for gross margins
!              if 1st april (day 91) is reference date.
!     likelihood of 2nd pick is indicated. currently 10 boll/m (bollz=10)
!     assumed to be worth picking, depends on price of cotton and cost of
!     picking, ask growers. should be part of management model.
!     date of 2nd pick and partition of yield between 1st and 2nd pick are
!     subjects for future development in a more comprehensive whole farm
!     management model.

      Use Infrastructure
      implicit none

      character  string*200            ! output string

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_harvest')

*- Implementation Section ----------------------------------
      call push_routine(myname)

!jh skip row correction
      g%alai_row = g%alai
      IF(g%NSKIP.GT.0)g%alai_row = g%alai*g%rs        ! lai in hedgerow

      IF(g%openz/(g%bollz+g%openz).GT.c%OPEN_DEF/100.
     :   .AND. g%n_def.EQ. 0) THEN
          g%J_DEF = g%iday                           ! day OPEN_DEF% open
!jh skip row correction
          IF(g%alai_row .LT. 0.5) then
          else IF(g%n_def.EQ.0) THEN
             g%n_def = 1                               ! 1st defoliant spray     !const
             g%i_def = g%iday                          ! g%day of 1st defol.
             write (string, '(4x, a, i4)')
     :                'Defoliant spray ', g%n_def
             call write_string (string)
          else
          endif
      else IF(g%n_def.EQ.1 .AND. g%iday-g%i_def.eq.10) THEN ! 10 days since 1st?
!jh skip row correction
          IF(g%alai_row.GT.0.2) THEN
             g%n_def=2                             ! 2nd defoliant spray      !const
             g%i_def2=g%iday                        ! g%day of 2nd defol
             write (string, '(4x, a, i4)')
     :             'Defoliant spray ', g%n_def
             call write_string (string)
          else
             g%j_pick = g%jdate                    ! date of picking
!jh need rs correction   ???
             if(g%bollz.LT.10) THEN                ! 10 bolls worth picking
                g%n_pick = 1                       ! count picks               !const
                write (string, '(4x, a, i4, a)')
     :                 'First Pick '
     :                , g%iday,' days from sowing. '
     :               // 'There are not enough bolls for a 2nd pick.'
                call write_string (string)
             else                ! 10 bolls worth picking
                g%n_pick = 2                      ! count picks                !const
                write (string, '(4x, a, i4, a)')
     :                 'First Pick '
     :                , g%iday,' days from sowing. '
     :               // 'There are enough bolls for a 2nd pick.'
                call write_string (string)
             ENDIF
          endif
      else IF(g%n_def.EQ.2 .AND. g%iday-g%i_def2.EQ.10) THEN
          g%j_pick = g%jdate                         ! date of picking
!jh need rs correction  ???
          IF(g%bollz.LT.10) THEN                       ! 10 bolls worth picking
             g%n_pick = 1                               ! count picks
             write (string, '(4x, a, i4, a)')
     :              'First Pick '
     :             , g%iday,' days from sowing. '
     :            // 'There are not enough bolls for a 2nd pick.'
             call write_string (string)
          else
             g%n_pick = 2                           ! count picks
             write (string, '(4x, a, i4, a)')
     :              'First Pick '
     :             , g%iday,' days from sowing. '
     :            // 'There are enough bolls for a 2nd pick.'
             call write_string (string)
          endif
      endif

!jh      IF(g%j_pick.NE.0 .AND. g%bollz.LT.1.0) THEN
      IF(g%j_pick.NE.0) THEN
              g%iend = 6                                 ! terminate crop                 !const
!c              write(2,103) jdate, iday
              write (string, '(4x, a)')
     :                'Crop terminated'
     :              //': first pick.'
              call write_string (string)
      ENDIF

      call pop_routine(myname)
      RETURN
      end subroutine


* ====================================================================
!      subroutine hfunc (i)
      subroutine ozcot_hfunc
* ====================================================================
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!      calculates daily heat units based on a 12.0 deg c base      c
!      temperature.  the heat unit sum is based an integrated      c
!      diurnal sin-wave with the maximum and minimum daily temp-   c
!      eratures as the peak and trough, respectively.  heat units  c
!      are not allowed to accumulate above a cutoff of 30.0 c. Changed to 40    c
!      the base (baset) and cutoff (hucut) temperatures are set in c
!      the 'init' subroutine.                                      c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      Use Infrastructure
      implicit none

      real pi
      real amp
      real tmax
      real zeta

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_hfunc')

*- Implementation Section ----------------------------------
      call push_routine(myname)


      PI=3.14159                                                       !const
!      tempav=(tempmn+tempmx)/2.
      AMP=g%tempmx-g%tempav
      TMAX=g%tempmx
      IF(TMAX.GE.c%HUCUT) TMAX=c%HUCUT
      IF(g%tempmn.GE.c%BASET) GO TO 10
      IF(TMAX.LE.c%BASET) GO TO 20
      ZETA=ASIN((c%BASET-g%tempav)/AMP)
      g%hunits=1./PI*(AMP*COS(ZETA)+(g%tempav-c%BASET)*(PI/2.-ZETA))
      GO TO 30
   10 g%hunits=(TMAX+g%tempmn)/2.-c%BASET
      GO TO 30
   20 CONTINUE
      g%hunits=0.0
   30 CONTINUE

      call pop_routine(myname)
      RETURN
      end subroutine


* ====================================================================
!      subroutine init
      subroutine ozcot_INITIAL()
* ====================================================================
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!      initializes variables in all common blocks                  c
!                                                                  c
!      key variables:                                              c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      Use Infrastructure
      implicit none

!pc      real soltro, spi, cumep, cumes, cumet, deltsw, dal
!pc      real bolln, bolnc
!pc      integer nfert
!      integer lastiday
      integer j
!      integer isdex
!      integer ncnt
!      integer lastfr
      integer k

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_initial')

*- Implementation Section ----------------------------------
      call push_routine(myname)

!      block data ozcot_initials


!      g%scboll = (/5.0,5.0,4.7,4.5,5.5
!     :            , .0, .0, .0, .0,7./) ! DP16,DP61,DP90,SIOK,SICA
!      g%respcon= (/.025, .025, .02306, .01593, .02306
!     :            , .0, .0, .0, .0,.025/) !  ditto
!      g%sqcon  = (/.021, .021, .02057, .02283, .02057
!     :            , .0, .0, .0, .0,.021/) !   ditto
!      g%fcutout= (/.4789, .4789, .4789, .5411, .4789
!     :            , .0, .0, .0, .0,.48/) !  ditto
!      g%flai   = (/1.0, 1.0, 0.87, 0.52, 0.87
!     :            , .0, .0, .0, .0,1./)       !  ditto
!jh      g%popcon =.03633

!jh      g%fburr = 1.23                     ! factor sc/boll to sc+burr/boll

!jh      data leaf_res_n_conc/0.02/

!      end subroutine

!------------------------------------------------------------------------------
!      'bclim' climate parameters
!------------------------------------------------------------------------------

!jh      g%mode=2 ! OZCOT1: -1=calib'n, 1=valid'n; OZCOT2: 2=full simulation

!psc      nszn = 1
!psc      i = 0

      g%iend=0
      g%iday=0
!pc   lastiday = 0
!psc      jdate=0
!psc      imet=0
      g%tempav=0.0
      g%alint=0.0
      g%hunits=0.0
!jh      g%hucut=40.0 ! ABH changed from 30 to 40 - 11/11/83
!jh      g%baset=12.0
      g%asoil=0.0
!pc   soltro=0.0
!jh v2001       g%stemp=0.0
!      eo=0.0
!      eos=0.0
!pc     spi=0.0
!pc     cumep=0.0
!pc     cumes=0.0
!pc     cumet=0.0

!      tempmx=0.0
!      tempmn=0.0
!      solrad=0.0
!      rain=0.0
!      newran= 0
!------------------------------------------------------------------------------
!     irrigation variables
!------------------------------------------------------------------------------
!jh v2001       DO 10 J=1,Max_rrig
!          igday(j)=0
!jh v2001           g%rrig(J)=0.0
!jh v2001 10    continue
!------------------------------------------------------------------------------
!      'bsoil' soil profile parameters
!------------------------------------------------------------------------------
      g%isw = 0  ! flag to initialise water balance in SOLWAT
!jh      g%ambda=1.44 ! Priestly-Taylor  daRosa 1983
!jh      g%ul1=1.4  ! limit for stage 1 g%es  daRosa 1983
!jh      g%cona=0.35 ! g%es rate in stage 2  daRosa 1983
!pc   isdex=0.0
      g%nlayr=0
      g%nrtlayr=0
!      sw=0.0
!      ul=0.0
!jh v2001      g%def=0.0
      g%nirr=0
      g%twater=0.
      g%ep = 0.0
!pc   deltsw=0.0
        g%rtsw=1.0
      DO 20 J=1,6
!      dlayr(j)=0.0
!     ullayr(j)=0.0
!      swlayr(j)=0.0
!jh v2001      g%trans(J)=0.0
   20 CONTINUE
!------------------------------------------------------------------------------
!      'bplnt' plant parameters
!------------------------------------------------------------------------------
!pc     ncnt=0
!jh      g%idate=0
      g%ilai=0
      g%iemrg=0
      g%isow=0
      g%isq=0
      g%ivar=0
      g%crspce=0.0
      g%ppm=0.0
      g%esum=0.0
      g%alai=0.0
      g%alai_row=0.0
!pc   dal=0.0
      g%shedlf=0.0
      g%frudw=0.0
      g%smi=0.0
      g%sdepth=0.0
      g%rtdep=0.0
      g%rtgrow=0.0
!------------------------------------------------------------------------------
!      'bnitr' soil nitrogen transformation parameters
!------------------------------------------------------------------------------
!pc   nfert=0
      g%uptakn=0.0
        g%availn=0.0
        g%initialN = 0.0

!      do 30 j=1,2
!      nday(j)=0
      g%snaplc=0.0
!   30 continue
!------------------------------------------------------------------------------
!      'fruits' parameters are numbers,weights,abscission counts
!      for bolls and squares.
!------------------------------------------------------------------------------
!pc   lastfr=0
      g%bgrvar=0.0
      g%dd=0.0
      g%ddmerg=0.0
      g%sumdd=0.0
      DO 40 J=1,Max_categories-1
      g%frucat(J)=0.0
      g%lfru(J)=0
   40 CONTINUE
      g%lfru(open_bolls)=0

      DO 50 J=1,Max_cohort
      g%dlai(J)=0.0
      g%ddw_l(J) = 0.0
      g%bpsum(j) = 0.0
      g%fyzage(J)=0.0
      g%fruno(J)=0.0
      g%fruwt(J)=0.0
      DO 50 K=1,7
      g%frmark(J,K)=0.0
      IF(J.LT.Max_categories-1) g%fmkcat(J,K)=0.0
   50 CONTINUE

!------------------------------------------------------------------------------
!      'totals' counts of squares, bolls, and sites at any given time
!------------------------------------------------------------------------------

      g%bload=0.0
      g%bollz=0.0
      g%openz=0.0
      g%openwt=0.0
      g%sites=0.0
!pc
      g%sites1 = 0.0
      g%squarz=0.0
!------------------------------------------------------------------------------
!      'index' stress and survival indices.
!------------------------------------------------------------------------------
      g%carcap=0.0
      g%carcap_c = 0.0
      g%carcap_n = 0.0
      g%cutout=0.0
      g%vnstrs=1.0
      g%fnstrs=1.0
      g%idayco = 0
      g%last_day = 0
!------------------------------------------------------------------------------
!      'counts'
!------------------------------------------------------------------------------
!      do 60 j=1,25
!      jco(j)=0
!      sqcnt(j)=0.0
!      blcnt(j)=0.0
!      opcnt(j)=0.0
!   60 continue
!------------------------------------------------------------------------------
!      'bpnitr' are for plant nitrogen.
!------------------------------------------------------------------------------
      g%uptakn=0.0
      g%vegn=0.0
!pc   bolln=0.0
      g%plantn=0.0
!pc   bolnc=0.0
      g%strucn=0.0
      g%frun=0.0
!------------------------------------------------------------------------------
!     /yield/ for output with yield
!------------------------------------------------------------------------------
      g%alaiz=0.
      g%ilaiz=0
      g%plntnz=0.
      g%iplntn=0
      g%sqzx = 0.
      g%isqzx = 0
!jh v2001      g%def_last=0.
!------------------------------------------------------------------------------
!     /pick/ variables to simulated defoliation and picking
!------------------------------------------------------------------------------
      g%j_pick=0
      g%n_pick=0
      g%n_def=0
      g%i_def=0
!jh      c%OPEN_DEF = 60.
!------------------------------------------------------------------------------
!     /sow/
!------------------------------------------------------------------------------
!jh      g%iwindow = 60     ! width of sowing window
!jh      g%sow_sw = 0.0     ! for subsoil water rule
!------------------------------------------------------------------------------
!     /drywt/ variables for simulation of dry weight increase
!------------------------------------------------------------------------------
!jh      g%a_root_leaf = 1.01 ! allometric constant root:leaf. Huxley's data 1964
!jh      g%a_stem_leaf = 1.25 ! allometric constant stem:leaf. Huxley's data 1964
!jh      g%e_par = 2.5        ! g%g/MJ Charles-edwards g%et al 1986
!jh      g%fburr = 1.23       ! ABH
!jh      g%specific_lw = 58.0 ! g%g/m2  GAC 71/72, Hoffman & Rawlins 1971, Ben-Porath
!jh      g%t_opt = 25.        ! Constable 1981
!jh      g%t_base = 8.        ! Constable 1981
!jh      g%wt_area_max = 150. ! Hesketh and Low 1968, Hoffman & Rawlins = 80.
!jh      g%wt_area_min = 30.0 ! Huxley 1964
!jh      g%embryo = 0.75      ! dry weight of seedling at emergence
!jh      g%f_leaf = 0.6       ! proportion of leaf dry weight at emergence
!jh      g%f_stem = 0.15      ! ditto for stem
!jh      g%f_root = 0.25      ! ditto for root  -  data by extrapolation from Huxlry

      g%bollgr = 0.0
      g%bper = 0.0
      g%dlai_pot = 0.0
      g%dw_boll = 0.0
      g%dw_leaf = 0.0
      g%dw_root = 0.0
      g%dw_stem = 0.0
      g%dw_total = 0.0
      g%total_n = 0.0
      g%reserve = 0.0
      g%res_cap = 0.0
      g%root_feedback = 0.0

      call pop_routine(myname)
      RETURN
      end subroutine


* ====================================================================
!      subroutine istsq (i,nszn)
      subroutine ozcot_istsq
* ====================================================================

!     identifies first square event and gives initial value for
!     fruno(1),sites & squarz.
!     delayed by water stress (smi < 0.25) and cold shock (tempmn < 11)
!     delayed
!     currently sets isq=i, should be isq=iday for consistency between seasons
!     when convenient, change and check all refs to isq

      Use Infrastructure
      implicit none

!      real ddisq


!      DIMENSION DDISQ(10)

!      data iszn/0/                         ! flag for new season
!      DATA DDISQ/9*420.,320./ ! Constable (pers. comm. 1983), 10=Empire

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_istsq')

*- Implementation Section ----------------------------------
      call push_routine(myname)

!jh      IF(g%idate.NE.0) GO TO 40

!     no counts - simulate first square

!      if(iszn.ne.nszn) then                ! is this a new season?
!          iszn = nszn                      ! reset flag
!          delay = 0.0                      ! delay in day degrees
!      end if

!psc    add delay to common block

      IF(g%smi.LT.c%smi_delay_crit) THEN               ! water stress delays squaring
          g%delay = g%delay+(1.-(g%smi/c%smi_delay_crit))*g%hunits
      END IF

      IF(g%tempmn.LT.c%cold_shock_delay_crit)
     :                        g%delay = g%delay+c%cold_shock_delay ! cold shock Constable (pers. comm)

!      IF(g%HAIL) g%delay = g%delay + p%TIPOUT + g%HAIL_LAG    ! tipping out delay
      IF(g%sumdd .LT. p%DDISQ + g%delay)then
         call pop_routine(myname)
         RETURN
      else
      endif
!      fruno(i-isow)=1.0*ppm  ! average plant has 1 square
!      fruno(i-isow)=0.5      ! as in 1983/84 version
      g%fruno(g%das)=1.0*g%ppm ! average plant has 1 square
      g%fruno(g%das)=0.5*g%ppm ! 50% plants have 1 square
!jh need rs correction ??? this has 0.5 fruit/m2 regardless of rowspacing
      g%fruno(g%das)=0.5       ! as in 1983/84 version
      GO TO 43


!      using counts

   40 CONTINUE
!      do 41 n=1,25
!      if(jco(n).eq.0)return !  no squares counted yet
!      if(i.lt.jco(n)+1)return
!      if(sqcnt(n+1).gt.0.)go to 42 ! when jco(n) eq i and squares at next count
!   41 continue
!      return
!   42 continue
!      fruno(i-isow)=sqcnt(n+1)/(jco(n+1)-jco(n))
!      next=n ! for actfru

!     square & site production on first day

   43 CONTINUE
!      squarz=squarz+fruno(i-isow) ! sum squares
!      sites=sites+fruno(i-isow) ! sum sites
!      isq=i                     ! should be isq=iday see above
      g%squarz=g%squarz+g%fruno(g%das) ! sum SQUARES
      g%sites=g%sites+g%fruno(g%das) ! sum g%sites
      g%isq=g%das                ! should be g%isq=g%iday see above

      call pop_routine(myname)
      RETURN
      end subroutine


* ====================================================================
!      subroutine laigen (i)
      subroutine ozcot_laigen
* ====================================================================

!     estimates current lai. generates new leaf area daily as a
!     function of dd and fruiting site production. initial area
!     is a function of dd to first square then a function of new
!     sites. relative rate of area expansion a function of dd
!     to 1st square, then a function of dsites .
!     AREAS ARE SQUARE M LEAF PER SQUARE M LAND (ALAI & DLAI)
!     OR SQUARE M PER SITE (dLdS & dLdS_max)

!     Revised by ABH April 1995

      Use Infrastructure
      implicit none


      real dlds
      real dlds_x
      real vlnstr
      real flfsmi
      real flfsmi_x
      real actrgr
      real alaix
      real ddleaf
      real range
      real hi
      real a
      integer index
      integer in
      integer l

!jh      DATA ACOTYL /.00035/
!jh      DATA RLAI /.00835/
!jh      DATA DLDS /0.0362/
!jh      DATA LEAF_RES_N_conc /0.02/

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_laigen')

*- Implementation Section ----------------------------------
      call push_routine(myname)


!------- initialise leaf area on day of emergence -----------------------------

      if(g%das.EQ.g%iemrg) THEN                    ! already emerged?
!        dlai(iemrg-isow)=acotyl*ppm            ! initial area
!        alai=dlai(iemrg-isow)
!        lastlf=iemrg-isow                      ! set index for oldest leaf
         g%dlai(g%iemrg)=p%acotyl*g%ppm      ! initial area
         g%alai=g%dlai(g%iemrg)
         g%lastlf=g%iemrg                  ! set index for oldest leaf
         call pop_routine(myname)
         RETURN
      endif

!------- Calculate water stress from SMI and VPD -------------------------------

        RANGE = g%vpd-15.0
        IF(RANGE .gt. 15.0) RANGE = 15.0

        HI = 0.5 + 0.4/15.0*RANGE         ! SMI below which stress occurs
        IF (HI .GT. 1.0) HI = 1.0
        IF (HI .LT. 0.01) HI = 0.01

        A = 1.0 - 0.7/15.0*RANGE          ! shapes of response to SMI
        IF (A .GT. 1.0) A = 1.0
        IF (A .LT. 0.01) A = 0.01

        FLFSMI = ozcot_stress(0.0, HI, A, g%smi)    ! stress factor
        FLFSMI_X = ozcot_stress(0.0, HI, A, 1.0)    ! stress factor

!------- calculate rate of leaf area expansion --------------------------------

!jh      A  =   0.1847  !
!jh      B1 =  -0.1165  ! constants for LAI calibration eqn
!jh      B2 =  -0.01514 ! 29 April 1988
!jh      B3 =   0.01984 ! expts WN8283 & 8384

!      dLdS_X = (c%a+c%b1*0.75+c%b2*g%vpd+c%b3*0.75*g%vpd) ! sqrt area/site, no water stress
!      if(dLdS_X.LT.0.) dLdS_X = 0.
!      dLdS_X = dLdS_X**2                      ! area per site, no water stress

!      dLdS = (c%a+c%b1*g%smi+c%b2*g%vpd+c%b3*g%smi*g%vpd) ! sqrt area per site
!      dLdS = (c%a+c%b1*g%smi*1.2+c%b2*g%vpd+c%b3*g%smi*1.2*g%vpd) !debug  ! sqrt area per site
!      dLdS = (c%a+c%b1*0.75+c%b2*g%vpd+c%b3*0.75*g%vpd) !debug  ! sqrt area per site
!      if(dLdS.LT.0.) dLdS = 0.
!      dLdS = dLdS**2                          ! area per site
!      FLFSMI = ozcot_stress(c%flfsmi_low
!     :                      ,c%flfsmi_high
!     :                      ,c%flfsmi_a
!     :                      ,g%smi) ! pre-squaring

!-------------------------------------------------------------------------------

      if(g%isq.EQ.0) THEN               ! crop not yet squaring

         ACTRGR=p%rlai                   ! actual RGR
!jh	      ACTRGR=ACTRGR*FLFSMI          ! water stress
         ALAIX=g%alai                  ! save previous LAI
         INDEX=IFIX(g%dd)              ! index for DO loop below to grow leaf

         DO 10 IN=1,INDEX
            ALAIX=ALAIX*(1.+ACTRGR)   ! grow leaf without water stress
10       continue
         g%dlai_pot=ALAIX-g%alai ! days increase without water stress

         ACTRGR=ACTRGR*FLFSMI          ! water stress
         ALAIX=g%alai                  ! save previous LAI again

         DO 11 IN=1,INDEX
            ALAIX=ALAIX*(1.+ACTRGR)   ! grow leaf with water stress
11       continue
         g%dlai(g%iday)=ALAIX-g%alai   ! days increase with water
      else                              ! crop now squaring
         dLdS = p%dLdS_max*FLFSMI        ! adjust for water stress
         IF(dLdS.LT.0.) dLdS = 0.
         dLdS = dLdS**2                ! area per site
         dLdS = dLdS*p%flai        ! adjust for variety, 87 MKI calib'n
         g%dlai(g%iday) = g%fruno(g%iday-1)*dLdS ! days incr in LAI
                                             ! without water stress
!         dLdS_X = p%dLdS_max*FLFSMI        ! adjust for water stress
         dLdS_X = p%dLdS_max*FLFSMI_X
         IF(dLdS_X.LT.0.) dLdS_X = 0.
         dLdS_X = dLdS_X**2                ! area per site
         dLdS_X = dLdS_X*p%flai    ! adjust for variety, 87 MKI calib'n
         g%dlai_pot = g%fruno(g%iday-1)*dLdS_X ! days incr in LAI
                                              ! with water stress
      endif

      VLNSTR = ozcot_stress(c%vlnstr_low
     :                      ,c%vlnstr_high
     :                      ,c%vlnstr_a
     :                      ,g%vnstrs)
      g%dlai(g%iday) = g%dlai(g%iday)*VLNSTR ! adjust for N stress
      g%dlai_pot = g%dlai_pot*VLNSTR ! adjust for N stress
      g%alai=g%alai+g%dlai(g%iday)

!*******senescence ***************************************************

      DDLEAF=ozcot_senlf(g%bload,g%alai,g%carcap_c,g%smi)
      if(g%n_def.EQ.1 .AND. g%iday-g%i_def.GT.7) DDLEAF = DDLEAF*0.33 ! 1st defol'n
      if(g%n_def.EQ.2 .AND. g%iday-g%i_def.GT.7) DDLEAF = DDLEAF*0.0 ! 2nd defol'n
      g%shedlf=0.
      g%leaf_res = 0.                 ! initialise for this g%day
      if(g%lastlf.EQ.0)GO TO 21       ! called after measured LAI finished

!     do 20 l=lastlf,i-isow           ! loop thro unshed leaves
      do 20 L=g%lastlf,g%das         ! loop thro unshed leaves
         IF(g%fyzage(L).LT.DDLEAF)GO TO 21 ! are this days leaves shed today?
         g%alai=g%alai-g%dlai(L)         ! reduce LAI
         g%shedlf=g%shedlf+g%dlai(L)     ! sum area of shed leaves
         g%dlai(L)=0.                    ! g%day,g%s area now zero
         g%dw_leaf = g%dw_leaf-g%ddw_l(L) ! reduce leaf dry matter
         g%leaf_res = g%leaf_res+g%ddw_l(L) ! sum this g%day's residues
         g%ddw_l(L) = 0.0                ! this g%day's leaf wt now zero
         g%lastlf=L+1 ! set index for oldest remaining leaves.
20    continue
21    continue

      g%leaf_res_n = g%leaf_res * c%leaf_res_n_conc ! N content of leaf residues
      g%N_LEAF = g%N_LEAF - g%leaf_res_n

      call pop_routine(myname)
      RETURN
      end subroutine


* ====================================================================
!      subroutine metdat2 (i,iend)
      subroutine ozcot_metdat2
* ====================================================================
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!      subroutine to check input climate data for missing or er-   c
!      roneous values.  input variable units are assumed input     c
!      and converted as follows:                                   c
!          temperature -      deg c*10 to deg c                    c
!          rainfall    -      mm/day*10 to cm/day                  c
!          rainfall    -      inches/day to cm/day                 c
!          solrad      -      ly/day (not converted)               c
!                                                                  c
!      unique variables:                                           c
!          qa = an upper boundary for solrad                       c
!          solrto = needed constant for net radiation in "evap"    c
!                                                                  c
!       when temperature (including wet and dry bulb) and          c
!       radiation are missing or not avaialble, ezstimates are     c
!       made as function of days since last rain.                  c
!       consequently a full suite of met data elements can be      c
!       generated from rainfall alone.                             c
!                                                                  c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      Use Infrastructure
      implicit none

!jh      integer nsince
!jh v2001       integer ndate
      real root
      real rclear
      real srad
      real wet_depress
!jh v2001       real date

!jh      DATA NSINCE/0/

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_metdat2')

*- Implementation Section ----------------------------------
      call push_routine(myname)

!         **** read met data  ***************************************

!       read(1,151,end=153) jdate,imyr,
!     *  rain,epan,tempmx,tempmn,tempdy,tempwt,wind,solrad
!151    format(4x,i3,5x,i4,f5.0,f4.1,2f4.0,2f4.1,f4.0,f4.0) ! new

!     **** climate data now read in as part of interface. ****

!psc        solrad = solrad / 0.04186
!psc        rain = rain / 10.0
        g%wind = 0.
        g%tempdy = 0.
        g%tempwt = 0.
!psc        epan = 0.


!jh
!jh v2001        IF(g%jdate.EQ.1) THEN               ! new year?
!jh v2001          g%mdpy = 365                     ! reset days per year
!jh v2001          IF((g%imyr/4*4).EQ.g%imyr) g%mdpy=366 ! leap year
!jh v2001        ENDIF


!       if(epan.eq.0. .or. epan.eq.99.9)      epan=epanx
!       epanx=epan

!       if(wind.eq.0. .or. wind.eq.999)  then
!           if(windx.ne.999) then
!               wind=windx
!           else
!               wind=0
!           endif
!       endif
!       windx=wind

!      go to 155
!153   iend=1 ! flag end of data
!      return

!155   continue
!psc      epan=epan/10.

!         **** check and convert rainfall (cm) ****

!        if(rain.lt.0.0) rain=0.0
!       rain=rain/100.0

!  local rain data

!       if(newran.eq.0) go to 100
!       if(jdate.ge.nrnsdy(1).or.jdate.le.nrnsdy(newran)) rain=0.0
!        do 10 j=1,newran
!         if(jdate.eq.nrnsdy(j)) rain=ransub(j)/10.
!10      continue
!100    continue

!  days since rain

!jh v2001       IF(g%isow.GT.0) THEN  ! from sowing to first boll
!jh v2001         IF(g%bollz.EQ.0.0) THEN
!jh v2001           g%rrig(rain_preboll) = g%rrig(rain_preboll)+g%rain     ! accumulate rainfall before bolling
!jh v2001         ELSE IF(g%bollz.GT.0.0 .AND. g%openz/(g%openz+g%bollz).LT.0.2)
!jh v2001      :  THEN ! bolling
!jh v2001           g%rrig(rain_postboll) = g%rrig(rain_postboll)+g%rain     ! accumulate rainfall in bolling
!jh v2001         ENDIF
!jh v2001       ELSE
!jh v2001           g%rrig(rain_fallow) = g%rrig(rain_fallow)+g%rain     ! accumulate rainfall in fallow
!jh v2001       ENDIF

      IF(g%rain.LE.0.1) THEN
        IF(g%NSINCE.LT.1) g%NSINCE=1
        g%NSINCE = g%NSINCE + 1
      ELSEIF(g%NSINCE.GT.1) THEN
        g%NSINCE=1                    ! 1st g%day of g%rain period, light g%rain
        IF(g%rain.GT.10.) g%NSINCE = 0 ! heavy g%rain
      ELSE
        g%NSINCE = 0                  ! g%rain period
      ENDIF
      ROOT =FLOAT(g%NSINCE)
      IF(ROOT.GT.5.) ROOT=5.
      IF(ROOT.GT.0.) ROOT = SQRT(ROOT)
      IF (g%NSINCE.GT.9) g%NSINCE=9


!      **** check solar radiation and fill in for missing data. ****
!      **** please notice that in the following lines location  ****
!      **** specific equations are used.    (cal/cm2)           ****

!  calculate extraterrestrial radiation at nars

!       xrad=(jdate+9)*(360./365.)*.0174533 ! day of year as angle in radians
!        qa=749.6+(302.4*sin(1.562+xrad)) ! extra terrestrial radiation(nars)

!       if(solrad.lt.0.0)solrad=0.0
!        if(solrad.gt.0.0 .and. solrad.ne.999.) go to 30

! estimate missing ground measured solar radiation

!       if(jdate.gt.135)go to 152
!       qqa=0.66-0.4708*exp(-0.75*nsince)        ! q/qa for days 1-135
!        if(nsince.le.1)qqa=0.4658-0.003485*rain
!        go to 160
!152    continue
!       if(jdate.ge.225)go to 154
!       qqa=0.5892-0.7986*exp(-1.219*nsince)     ! q/qa for days 136-225
!        if(nsince.le.1)qqa=0.1382+0.2777*exp(-0.04375*rain)
!        go to 160
!154    continue
!       qqa=0.63324-0.7693*exp(-1.0*nsince)
!        if(nsince.le.1)qqa=0.2148+0.2087*exp(-0.01875*rain)
!160    continue
!       solrad=qa*qqa           ! est of ground rad =f(days since rain)
!        solrad_min = qa*0.18      ! minimum - 0.18 from brutsart(1982)
!       if(solrad.lt.solrad_min)solrad=solrad_min
!30     continue

!   actual solar/clear day solar radiation ratio for longwave estimate

        RCLEAR=551.52+246.40*SIN(0.0172*(g%jdate+99.96)) !CLEAR g%day SOL g%rad(NARS)
        SRAD=g%solrad
        IF(SRAD.GT.RCLEAR) SRAD=RCLEAR
        g%solrto=SRAD/RCLEAR
!        solrad=srad

!       **** check and convert air temperatures (deg c) ****

!       tempmx=tempmx/10.0
!       tempmn=tempmn/10.0

!      if(tempmx.eq.0. .or. tempmx.eq.99.9) then

!  estimate missing tempmx

!       tmaxxx=26.24+8.325*sin(1.172+xrad)   ! tmax 8 or more days after rain
!       ftmax=1.03-0.1812*exp(-0.1953*nsince)! tmax/tmaxxx
!       tempmx=tmaxxx*ftmax               ! tmax=f(days since rain)
!       if(rain.gt.4.0)tempmx=tmaxxx*.83
!       if(rain.gt.5.0)tempmx=tmaxxx*.8

!      endif

!      if(tempmn.eq.99.9) then

!  estimate missing tempmn

!       tminxx=11.45+8.144*sin(1.078+xrad)             ! tmin on dry days
!       if(nsince.le.1)tminxx=13.47+5.949*sin(1.+xrad) ! tmin on wetdays
!       if(nsince.eq.2)ftmin=.993                      ! first day after rain
!       if(nsince.gt.2)ftmin=.925+.01321*nsince
!       if(nsince.le.1)ftmin=1.003+.005169*rain-.0001039*rain**2
!       tempmn=tminxx*ftmin                         ! estimate of tmin

!      end if

!       estimate wet and dry bulb when odd day missing

      IF((reals_are_equal(g%tempdy,0.0)) .OR.
     :   (reals_are_equal(g%tempdy,99.9))) THEN
          g%tempdy = -.54+0.57*g%tempmx+0.40*g%tempmn
      ENDIF
      IF((reals_are_equal(g%tempwt,0.0)) .OR.
     :   (reals_are_equal(g%tempwt,99.9))) THEN
          WET_DEPRESS = -3.103+0.28*g%tempmx-0.07*g%tempmn+0.62*ROOT
          IF(WET_DEPRESS.LT.0.) WET_DEPRESS=0.
          g%tempwt = g%tempdy-WET_DEPRESS
      ENDIF

!          **** calculate soil heat flux (cal/cm2) ****

!jh v2001         NDATE=g%jdate+183               ! CONVERT TO NORTHERN HEMISPHERE
!jh v2001         IF(NDATE.GT.g%mdpy) NDATE=NDATE-g%mdpy
!jh v2001         DATE=real(NDATE)
!jh v2001         g%g=1.7+14.6*SIN(0.0172*(DATE-51.0))           ! TEMPLE,TEXAS
!jh v2001         IF(g%g.LT.0.0) g%g=0.0

!      call hfunc (i) ! calculate heat units or daydegrees
      CALL ozcot_hfunc
!jh v2001       g%stemp=(g%tempmx+g%tempmn)/2.0*g%asoil
!      call evap (i) ! calculate potential evaporation

      call ozcot_evap
      call pop_routine(myname)
      RETURN
      end subroutine


* ====================================================================
      SUBROUTINE ozcot_n_fertilise (APPLIED,availn,APPLIED_AVAIL)
* ====================================================================
!c
!c      simulates uptake of fertiliser nitrogen. assumes that there is an upper
!c      limit to the amount of nitrogen a crop can take up and the rate of uptake
!c      or recovery of fertiliser n decreases linearly from a maximum initial
!c      value to zero when uptake limit is reached (basinski et al 1975, cot
!c      gr rev). assume intial rate is 1.0 (100% recovery) and maximum uptake
!c      is 240 kg/ha.
!c
      Use Infrastructure
      implicit none

      real uptakn_max
      real rate_reducer
      real availnx
      real fraction
      real applied
      real applied_avail
      real availn
      integer n
      integer nkg

       DATA UPTAKN_MAX /240./
!c
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_n_fertilise')

*- Implementation Section ----------------------------------
      call push_routine(myname)

       NKG = IFIX(APPLIED+0.5)             !  integer of kgs, index for DO loop
       RATE_REDUCER = 1./UPTAKN_MAX    !  recovery decreases with uptake
!c
       AVAILNX   = availn            ! available N before application
       DO 1000 N=1,NKG
           FRACTION = max(0.0, 1.0-RATE_REDUCER*availn) ! fraction of next kg available
           availn = availn + FRACTION
1000  continue
       APPLIED_AVAIL = availn-AVAILNX ! N applied now that will be available
!c
       call pop_routine(myname)
       RETURN
       end subroutine


* ====================================================================
      subroutine ozcot_overload
* ====================================================================

!-------------------------------------------------------------------------------
!     simulates abscission or abortion of older fruit under extreme stress
!     ie when boll load exceeds carrying capacity.
!-------------------------------------------------------------------------------

      Use Infrastructure
      implicit none

      real over_c
      real over_n
      real fload
      real capacity
      real excess
      real available
      real abort
      integer cohort
      integer icat
      integer age

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_overload')

*- Implementation Section ----------------------------------
      call push_routine(myname)

!----- determine if overload called for c or n ---------------------------------

      OVER_C = 999.                           ! very large when g%carcap_c=0.
      IF(g%carcap_c .GT. 0.) OVER_C = g%bload/g%carcap_c
      OVER_N = 999.                           ! very large when g%carcap_n=0.
      IF(g%carcap_n .GT. 0.) OVER_N = g%bollz/g%carcap_n

      IF(OVER_C .GT. OVER_N) THEN             ! Cc is limiting
          FLOAD = g%bload                     ! fruit load
          CAPACITY = g%carcap_c               ! cpacity
      ELSE                                    ! N is limiting
          FLOAD = g%bollz                     ! fruit load
          CAPACITY = g%carcap_n               ! cpacity
      ENDIF

!----- count days fruit load exceeds capacity ---------------------------------

      IF(g%iday-g%last_iday.GT.1)g%idayco = 0 ! reset for break
      IF(FLOAD.GT.CAPACITY) g%idayco = g%idayco+1 ! count days FLOAD>g%carcap
      g%last_iday = g%iday                    ! g%last g%day counted
      IF(g%idayco.LT.3)then
         call pop_routine(myname)
         RETURN                 ! buffered by reserves
      else
      endif

!----- compute excess fruit and buffer effect ---------------------------------

      EXCESS = FLOAD-CAPACITY                 ! bolls in excess of Cc supply
      EXCESS = EXCESS*0.1                     ! damp effect for carbon stress

!----- loop through arrays to find available fruit ----------------------------

      DO 2 cohort=g%lfru(Small_bolls),g%lfru(Inedible_bolls)-1,-1           ! loop through categories 5, 6, 7
        IF(cohort.LT.1) GO TO 1                    ! no fruit to abort
        ICAT = Flowers
        IF(cohort.LE.g%lfru(Small_bolls)) ICAT = Small_bolls
        IF(cohort.LE.g%lfru(Medium_bolls)) ICAT = Medium_bolls
        IF(cohort.LE.g%lfru(Large_bolls)) ICAT = Large_bolls
        IF(g%fruno(cohort).EQ.0.)GO TO 2                    ! no fruit
        AVAILABLE = g%fruno(cohort)                         ! fruit available to abort
        DO 10 age=age1,age6
            AVAILABLE = AVAILABLE-g%frmark(cohort,age)      ! less fruit marked
10      continue


        IF(ICAT.EQ.Large_bolls
     :    .AND.g%fruwt(cohort)/g%fruno(cohort).LT.0.1)THEN ! fruit not grown yet ?
           g%frmark(cohort,age6) = g%fruno(cohort)                  ! abort such fruit
           AVAILABLE = AVAILABLE-g%frmark(cohort,age6)         ! adjust fruit available
        ENDIF

        IF(AVAILABLE.GT.0.)THEN
          AVAILABLE = AVAILABLE*0.1                    ! damp effect
          ABORT = AVAILABLE                            ! abort available fruit
          IF(ABORT.GT.EXCESS) ABORT=EXCESS             ! limit to requirement
          g%frmark(cohort,age6) = g%frmark(cohort,age6)+ABORT
          g%fmkcat(ICAT,age6) = g%fmkcat(ICAT,age6)+ABORT
          EXCESS = EXCESS-ABORT                        ! reduce excess no. bolls
          IF(EXCESS.LE.0.) GO TO 1                     ! excess depleted
        ENDIF
2     continue

1     continue

!-------------------------------------------------------------------------------

      call pop_routine(myname)
      RETURN
      end subroutine


* ====================================================================
        real FUNCTION ozcot_satvp(Tdeg)
* ====================================================================

        Use Infrastructure
      implicit none

        real Tdeg

        real tabs
        real tr
        real trlog
        real ts
        real tt
        real ewslog
        real ew

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_satvp')

*- Implementation Section ----------------------------------
      call push_routine(myname)

        TABS=Tdeg+273.16
        tr=373.16/TABS
        TRLOG=ALOG10(tr)
        tr=tr-1.
        TS=(10.**(11.344*(1.-TABS/373.16))-1.)/10.**7.
        TT=(10.**(-3.49149*tr)-1.)/10.**3.
        EWSLOG=ALOG10(1013.246)
        EW=-7.90298*tr+5.02808*TRLOG-1.3816*TS+8.1328*TT+EWSLOG
        ozcot_satvp=10.**EW

        call pop_routine(myname)
        RETURN
        end function

* ====================================================================
      real FUNCTION ozcot_senlf(bload,alai,carcap_c,smi)
* ====================================================================

!     estimates leaf longevity. ranges between 833 dd & 1110 dd
!     reduced by water stress, nitrogen stress, boll load and self shading of
!     canopy when lai gt 3.

      Use Infrastructure
      implicit none


      real fb
      real fw
      real fn
      real fl
      real carcap_c
      real bload
      real alai
      real smi
      real f

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_senlf')

*- Implementation Section ----------------------------------
      call push_routine(myname)

      FB=1.
      FW=1.                          ! water stress factor
      FN=1.                          ! nitrogen stress factor
      FL=1.                          ! LAI  factor
      IF(carcap_c.GT.0.)FB=1.-bload/carcap_c ! reduce by boll load
      IF(FB.GT.1.)FB=1.
      IF(FB.LT.0.)FB=0.
      FW=ozcot_stress(c%fw_low,c%fw_high,c%fw_a,smi) ! effect of water stress
      ozcot_senlf=833.+277.*FB*FW*FN

!     proposal 3/1/97 try when convenient ABH

      FN=ozcot_stress(0.75,1.0,1.0,g%FNSTRS) ! nitrogen stress 30 Dec 1992
!jh skip row correction ??
      g%alai_row = g%alai
!      IF(g%NSKIP.GT.0)g%alai_row = g%alai*g%rs        ! lai in hedgerow

      IF(g%alai_row.GT.3.) FL = 1.0-(g%alai_row-3.0)/3.0  ! effect of LAI > 3
      IF(FL.GT.1.)FL=1.
      IF(FL.LT.0.)FL=0.

      F = MIN(FB*FW,FN,FL)
!      SENLF=833.+277.*F

      call pop_routine(myname)
      RETURN
      end function

!jh      SUBROUTINE SNBAL(I)
      SUBROUTINE ozcot_SNBAL

C      This subroutine estimates potential total N uptake of the crop (AVAILN).
C      It does not maintain a daily available N balance; this will be done in
C      a later version.
C      Fertiliser and non-fertiliser N is entered as input. Non-fertiliser n
C      is immediately added to AVAILN. A fraction of fertiliser N is added on
C      day of application in S/R N_FERTILISE, the fraction being a function of
C      N already available. All available N is reduced by waterlogging.
C      Fertiliser N is reduced by low available soil water content is.
C      Day of application of N fertiliser, NDAY(J) from CINPUT2 & AGRON.INP,
C      can be day of year (+ve) or days after sowing (-ve), assigned to local
C      variable JNAPLC(J) on first day of season or day of sowing.

      Use Infrastructure
      implicit none

!jh      INTEGER JNAPLC(2)                         ! local variable, day of applcn
      real    APPLIED_AVAIL
      real    REDUCTION

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_SNBAL')

*- Implementation Section ----------------------------------
      call push_routine(myname)

C---- reset variables for new season ------------------------------------------

      IF(g%das.EQ.1) THEN                           ! start of new season?
          g%APPLIED_N = 0.                        ! reset for new season
          g%TOTAL_APPLIED = 0.                    !       ditto
!jh          DO J=1,NFERT
!jh             JNAPLC(J) = 0                      ! reset
!jh             IF(NDAY(J).GT.0) JNAPLC(J)=NDAY(J) ! day of applcn as of year
!jh          ENDDO
      ENDIF

!jhC---- find day of N application if tied to sowing date ------------------------

!jh      IF(I.EQ.ISOW) THEN                        ! sown this day?
!jh         JSOW = ISOW+IMET-1                     ! sowing as day of year
!jh         IF(JSOW.GT.MDPY) JSOW=JSOW-MDPY        ! JSOW next year? if so adjust
!jh         DO J=1,NFERT
!jh             IF(NDAY(J).LT.0 .AND. SNAPLC(J).GT.0.) THEN
!jh                 NAFTER = -NDAY(J)              ! make +ve
!jh                 JNAPLC(J) = JSOW+NAFTER        ! day of applcn as of year
!jh                 IF(JNAPLC(J).GT.MDPY) JNAPLC(J)=JNAPLC(J)-MDPY ! next year?
!jh             ENDIF
!jh         ENDDO
!jh      ENDIF

C--- update available N if fertiliser applied ---------------------------------

!jh      IF(JDATE.EQ.JNAPLC(J)) THEN               ! apply N this day?
         if (g%snaplc.gt. 0.0) then
          CALL ozcot_N_FERTILISE (g%SNAPLC,g%AVAILN,APPLIED_AVAIL) ! if so, do it
          g%TOTAL_APPLIED = g%TOTAL_APPLIED+g%SNAPLC           ! total N applied
          g%APPLIED_N = g%APPLIED_N+APPLIED_AVAIL   ! total applied N avail
      ENDIF
20    CONTINUE

C--- adjust available N during active fruiting after Hearn & Constable 1984-----
      IF(g%SQUARZ.GT.0.0 .AND. g%OPENZ. EQ.0.)THEN       ! active fruiting?

C        IF(DEF.LT.2.5) THEN                          ! waterlogging
!jh        IF(g%SW/g%UL.GT.c%WATLOG_N) THEN                    ! waterlogging 28/5/96
        IF(g%wli.GT.c%WATLOG_N) THEN                    ! waterlogging 28/5/96
            IF(g%AVAILN.GT.30.) THEN
                g%AVAILN = g%AVAILN-0.983                ! Hearn & Constable
            ELSE
                g%AVAILN = g%AVAILN*0.99                 ! Hearn Constable
            ENDIF
        ENDIF

        IF(g%SMI.LT.0.3 .AND. g%APPLIED_N.GT.0. AND. g%BOLLZ.GT.0.) THEN ! dry?
           REDUCTION =  0.0316*g%TOTAL_APPLIED         ! reduces avail N
           g%APPLIED_N =g%APPLIED_N-REDUCTION            ! reduce applied N avail
           IF(g%APPLIED_N.LT.0.) g%APPLIED_N=0.
           g%AVAILN = g%AVAILN - REDUCTION                 ! reduce available N
           IF(g%AVAILN.LT.0.0)g%AVAILN=0.0
        ENDIF
      ENDIF


      call pop_routine(myname)
      RETURN
      end subroutine

* ====================================================================
!jh      SUBROUTINE SOLWAT (I)
      subroutine ozcot_solwat
* ====================================================================

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!      CALCULATES THE SOIL WATER BALANCE AS A FUNCTION OF SOIL     C
!      LAYER DEPTH.  MODIFIED FROM THE MODEL OF RITCHIE (1972).    C
!                                                                  C
!     Order of execution - numbers are original order:             C
!          Initialise in 1st season                                C
!          Reset intial SW if specified                            C
!        1  CALL CULTIVATE                                         C
!        4  Call SEVAP    gives ES                                 C
!        5  CALL EVAPOTRANS uses SMI, gives EP & ET                C
!        7  CALL SWBAL_ET   removes EP & ES from layers            C
!        8  CALL SUM_SW - sum layers as SW gives DEF               C
!        2  CALL IRRIGATE uses DEF adds irrig water to rainfall    C
!        3  CALL RUNOFF   uses SW, gives Q                         C
!        6  CALL SWBAL_ADD  adds rain & irrigation to layers       C
!        8  CALL SUM_SW - sum layers as SW gives DEF               C
!        9  CALL WATER_INFO information for output                 C
!       10   CALL INDICES  SMI & WLI                               C
!       11   CALL DRAINAGE                                         C
!       12   CALL SKIPWATER                                        C
!                                                                  C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      Use Infrastructure
      implicit none

!-------INITIALISING -----------------------------------------------------------

!?        IF(I.EQ.1) JIR = 1
        IF(g%isw.NE.1) THEN           ! replaces INITIAL.GT.1 for OZCOT2&4 May89
            g%isw = 1
            g%smi=g%swlayr(1)/g%ullayr(1)
        ENDIF

!------- reset initial soil water ------------------------------------------

!------ REMOVE WATER FROM LAYERS 1&2 DURING LAND PREPARATION -------------------

      call ozcot_EVAPOTRANSP               ! uses SMI, gives EP & ET
!jh      CALL SWBAL_ET(I)               ! removes EP & ES from layers
      call ozcot_SWBAL_ET               ! removes EP & ES from layers
      call ozcot_SUM_SW                    ! sums SW and gives DEF
!jh      CALL IRRIGATE (RAINSI)         ! uses DEF
!jh      CALL RUNOFF (RAINSI,RAINEF)    ! uses SW, gives Q
!jh      CALL SWBAL_ADD (RAINEF)        ! adds rain & irrigation to layers
!jh      CALL SUM_SW                    ! sums SW and gives DEF
!jh      CALL WATER_INFO (I)
      call ozcot_INDICES                   !gives SMI & WLI
!jh      IF (g%wli.GT.WATLOG_N) call DRAINAGE
      IF(g%nskip.GT.0) call ozcot_SKIPWATER  ! for skip row

  500   CONTINUE

        RETURN
        end subroutine


!-------------------------------------------------------------------------------

      subroutine ozcot_SKIPWATER

!     This subroutine deals with use of soil water in the skip row.
!     At the start of a drying cycle water is drawn only from the plant row,
!     and SMI calculated for the plant row, until the supply is limiting EP
!     (shown by F_LIMITING < 1, from WATCO).
!     With supply limiting water is drawn from skip until it is depleted
!     to same level as plant row (shown when SMI for plant+skip =<
!     SMI for plant row) where upon water is drawn from plant and skip rows.
!     Until SMI for plant+skip < SMI for plant row, SMI for plant row is
!     effective SMI. When profile rewetted and drying cycle ends, procedure
!     starts again with water drawn from plant row until supply limited.
!     WLI (waterlogging index) adjusted for plant row,
!     assuming water logging is limited to plant row.

      Use Infrastructure
      implicit none


      real     smi_row
      real     smi_pre

!jh      LOGICAL USESKIP/.FALSE./          ! water used from skip when TRUE

!jh      SMI_IN = g%smi                    ! to write o/p in development

      IF(.NOT. g%useskip) THEN
          g%smi_row=1+(g%smi-1)*g%rs        ! SMI in plant row
          IF(g%f_limiting.LT.1.) THEN       ! supply limiting EP?
              g%useskip = .TRUE.            ! yes, water from skip
              g%smi_pre = g%smi             ! save value of SMI
!              F_SMI = g%smi_row/g%smi      ! to adjust SMI when water in skip used
          ENDIF
          g%smi = g%smi_row                   ! effective SMI
      ELSE
          IF(g%smi.GT.g%smi_pre) THEN         ! profile rewetted?
              g%useskip = .FALSE.             ! yes, rewetting occurs
              g%smi_row=1+(g%smi-1)*g%rs      ! SMI in plant row
              g%smi = g%smi_row               ! effective SMI
          ELSE
              g%smi = MIN(g%smi,g%smi_row)    ! no, drying continues
!              g%smi = g%smi*F_SMI            ! adjust SMI when using water in skip
          ENDIF
      ENDIF

      g%WLI = 1-g%rs*(1-g%WLI)                ! water logging index in row

!      WRITE (4,10) SMI_IN,SMI,g%f_limiting,g%smi_pre,F_SMI,g%useskip
10    FORMAT(5F5.2,L5)

      RETURN
      end subroutine


* ====================================================================
      subroutine ozcot_EVAPOTRANSP
* ====================================================================

!     calculates EP, adds to ES, to get ET, limits to EO

      Use Infrastructure
      implicit none


      g%alai_row = g%alai
        IF(g%nskip.GT.0) g%alai_row = g%alai*g%rs  ! lai in hedgerow - ABH 5/11/96

        IF(g%alai_row.GT.3.) THEN
            g%ep=g%eo-g%es
        ELSE IF(g%alai_row.GT.1.6)THEN
            g%ep=(0.08+0.3066*g%alai_row)*g%eo     ! L.Mateos 1987, ABH 1988
        ELSE
            g%ep=(1.-EXP(-0.5186*g%alai_row))*g%eo ! L.Mateos 1987, ABH 1988
        ENDIF

        IF(g%alai_row.EQ.0.0) g%ep=0.0
        IF(g%ep.LT.0.) g%ep=0.

        IF(g%nskip.GT.0) THEN
            g%ep = g%ep/g%rs                   ! EP on ground area basis
!jh            g%alai = g%alai/g%rs               ! restore LAI to ground area basis
        ENDIF

!------ LIMIT EP USING WATCO(SMI) STRESS FACTOR --------------------------------

        g%et=g%es+g%ep
        IF(g%eo.LT.g%et) THEN
            g%et=g%eo
            g%ep=g%et-g%es
            IF(g%ep.LT.0.0)g%ep=0.0
        ENDIF
        g%F_LIMITING = ozcot_watco(g%smi,g%eo,0.4,0.) ! factor when supply limiting ep
        g%ep=g%ep*g%F_LIMITING
        g%et=g%es+g%ep


      RETURN
      end subroutine


* ====================================================================
      subroutine ozcot_INDICES
* ====================================================================

!     calculates SMI AND WLI

      Use Infrastructure
      implicit none

      real depth
      real rtul
      integer L
      real smi_rt
      real smi_30

        DEPTH=0.
        g%rtsw=0.
        RTUL=0.
        g%nrtlayr = 0.0

        DO L=1,g%nlayr
          DEPTH=DEPTH+g%dlayr_cm(L)
          g%nrtlayr = L
          IF(g%rtdep.LE.DEPTH) GO TO 460
          RTUL=RTUL+g%ullayr(L)*g%dlayr_cm(L)
          g%rtsw=g%rtsw+g%swlayr(L)*g%dlayr_cm(L)
        ENDDO

        GO TO 470

460     g%rtsw=g%rtsw+g%swlayr(L)*(g%rtdep+g%dlayr_cm(L)-DEPTH)
        RTUL=RTUL+g%ullayr(L)*(g%rtdep+g%dlayr_cm(L)-DEPTH)
470     SMI_RT=g%rtsw/RTUL                        ! SMI in root zone
        SMI_30=g%swlayr(1)/g%ullayr(1)              ! SMI in top 30cm
        IF(g%dlayr_cm(1).GE.30.) GO TO 480
        SMI_30 = (SMI_30*g%dlayr_cm(1)
     :         + (g%swlayr(2)/g%ullayr(2))*(30.-g%dlayr_cm(1)))/30.
480     g%smi=AMAX1(SMI_RT,SMI_30)
!jh        g%smi=SMI_RT   !debug
        g%smi = min(1.0, g%smi)
!jh      print*, smi_rt, g%rtdep
!jh        g%wli = g%sw/g%UL
!jhtemp
!jh        g%wli = SMI_RT
        g%wli = g%smi
!        g%smi = 0.86 !debug
!        g%wli = 0.86 !debug

! correction of indices for soilwat2 water movements
!jhtemp        g%smi = -0.107 + 1.187*g%smi
!jhtemp        g%smi = max(0.0,min(1.0,g%smi))                        ! waterlogging index
!jhtemp        g%wli = -0.107 + 1.187*g%wli                         ! waterlogging index
!jhtemp        g%wli = max(0.0,min(1.0,g%wli))                        ! waterlogging index

      RETURN
      end subroutine

* ====================================================================
      subroutine ozcot_SUM_SW
* ====================================================================

!     calculates SW in each layer & sums down profile

      Use Infrastructure
      implicit none

      integer L

        g%sw=0.0

        DO L=1,g%nlayr
            g%sw=g%sw+g%swlayr(L)*g%dlayr_cm(L)
        ENDDO

!jh v2001        g%def = g%UL-g%sw

      RETURN
      end subroutine




* ====================================================================
        real FUNCTION ozcot_stress(LOW,HIGH,A,STRS)
* ====================================================================
        Use Infrastructure
      implicit none

        real HIGH
        real A
        real STRS

!       computes or adjusts a factor.
!       input is state variable strs with upper and lower limits, high,low.
!       output is between 0 and 1.
!       a =  1 gives factor a linear fn of ratio of strs to high - low
!       a gt 1 accentuates effect of strs on value
!       a lt 1 damps effect.

        REAL LOW

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_stress')

*- Implementation Section ----------------------------------
      call push_routine(myname)

      ozcot_stress=(STRS-LOW)/(HIGH-LOW)
      IF(ozcot_stress.GT.1.) then
         ozcot_stress=1.
      elseIF(ozcot_stress.LE.0.) then
         ozcot_stress=0.
      else
         ozcot_stress=(1.-(1.-ozcot_stress)**A)
      endif
!      ozcot_stress=1. !debug
      call pop_routine(myname)
      RETURN
      end function
* ====================================================================
      real FUNCTION ozcot_survive(CAPACITY,bload)
* ====================================================================
!
!     20070911 DBJ  added p%BckGndRetn  to allow varietal adjustment for
!                   background retention.
! ---------------------------------------------------------------------
      Use Infrastructure
      implicit none

      real CAPACITY
      real bload
      real a
      real b

!     estimates survival of fruit as function of boll load.

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_survive')

*- Implementation Section ----------------------------------
      call push_routine(myname)

      ozcot_survive = 0.
      IF(CAPACITY.EQ.0.)then
         call pop_routine(myname)
         RETURN
      else
      endif
      A=1.0 ! intercept of survival function                 !const
      B=A/CAPACITY ! slope of survival function
      ozcot_survive=A-B*bload ! prortion surviving
      IF(ozcot_survive.LT.0.)ozcot_survive=0.
      IF(ozcot_survive.GT.1.)ozcot_survive=1.
      IF(p%BckGndRetn.GT.0.0) THEN
         ozcot_survive = ozcot_survive*p%BckGndRetn  ! varietal specific background retention (1 - sub-threshold shedding)
      ELSE
         ozcot_survive = ozcot_survive*0.8  ! background, sub-threshold shedding default value
      ENDIF

      call pop_routine(myname)
      RETURN
      end function


* ====================================================================
!       subroutine swbal(i,rainef)
        subroutine ozcot_swbal_et
* ====================================================================


!   **** soil & plant water balance including rain and soil evaporation, ****
!   **** beginning with the top soil layer.                      ****

      Use Infrastructure
      implicit none

      real ux
      real epcoef
      real uob
      real sum
!jh v2001      real stran
      real depth
      real tdepth
      real epd
      real swlr
      real dswmx
      integer l

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_swbal')

*- Implementation Section ----------------------------------
      call push_routine(myname)


!       percol=rainef
!       exes=es
        UX=0.0
        IF(g%smi.GE.c%epcoef_smi_crit)EPCOEF=c%epcoef1            !  W*N                 !const
        IF(g%smi.LT.c%epcoef_smi_crit)EPCOEF=c%epcoef2            !  82/83               !const
        UOB=g%ep/(1.-EXP(-EPCOEF))
        SUM=0.0
!jh v2001        STRAN=0. ! sum of g%trans(L) down profile
        DEPTH=0.
        TDEPTH=0.
        EPD=0.0
!       w=0.

!***********************************************************************

          DO 10 L=1,g%nlayr
!psc
!psc          swlayr(l)=swdep(l)/10./dlayr_cm(l)
!psc     *                 -(duldep(l)/10./dlayr_cm(l)-ullayr(l))
!psc
!jh v2001          g%trans(L)=0.
            SWLR=g%swlayr(L)*g%dlayr_cm(L)
!           swmax=ullayr(l)*dlayr_cm(l)
!           if(percol.eq.0..and.exes.eq.0.)go to 2

!**** rain percolates layer 'l'

!       swlr=swlr+percol
!       percol=0.
!       if(swlr.le.swmax)go to 1
!       percol=swlr-swmax ! surplus percolates to next layer
!       swlr=swmax        ! this layer full

!**** extract es from layer 'l'

! 1     swlr=swlr-exes    ! extract es from this layer
!       exes=0.
!       if(swlr.ge.0.)go to 2
!       exes=-swlr        ! es carried down to next layer
!       swlr=0.

!**** extract ep from this layer

!2        continue
          DEPTH=DEPTH+g%dlayr_cm(L)         ! moved here 29/6/88
!         if(i.lt.iemrg) depth=0.0       !       ditto
          IF(g%das.LT.g%iemrg) DEPTH=0.0   !       ditto
          IF(g%rtdep.LE.TDEPTH) GO TO 11 !       ditto
          IF(g%rtdep.LT.DEPTH) DEPTH=g%rtdep !       ditto
          SUM=UOB*(1.-EXP(-EPCOEF*DEPTH/g%rtdep))
          DSWMX=SUM-UX
          SWLR=SWLR-DSWMX    ! EXTRACT g%ep FROM THIS LAYER
          IF(SWLR.GE.0.) GO TO 3
          EPD=SWLR
          SWLR=0.
!jh v2001          DSWMX=g%dlayr_cm(L)*g%swlayr(L)
!jh v2001          g%trans(l)=dswmx
3         continue
          TDEPTH=DEPTH
          UX=SUM+EPD   ! EPD CORRECTS UX IF LAYER IS DRY
          EPD=0.0
11        continue ! moved from after next statement 29 jun 88
          g%swlayr(L)=SWLR/g%dlayr_cm(L)          ! water_uptake
!jh v2001          STRAN=STRAN+g%trans(L)
!jh v2001          g%setlyr(L)=g%setlyr(L)+STRAN ! cumulative transp. thro season down profile
10      continue

        call pop_routine(myname)
        RETURN
        end subroutine


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!      water stress function for root growth.                      c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

* ====================================================================
      real FUNCTION ozcot_watco(smi,eo,X3,X1)
* ====================================================================
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!      water stress function for root growth.                      c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      Use Infrastructure
      implicit none

      real smi
      real eo
      real X3
      real X1
      real x2
      real y0
      real slope

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_watco')

*- Implementation Section ----------------------------------
      call push_routine(myname)

      X2=X3*eo/0.8
      IF(X2.GT.1.0) X2=1.0
      IF(smi.LT.X2) GO TO 20
      ozcot_watco=1.0
      GO TO 30

   20 CONTINUE
      SLOPE=1.0/(X2-X1)
      Y0=1.0-SLOPE*X2
      ozcot_watco=SLOPE*smi+Y0
      IF(ozcot_watco.LT.0.0) ozcot_watco=0.0

   30   CONTINUE

        call pop_routine(myname)
        RETURN
        end function


* ====================================================================
!      subroutine yield(nszn,iend)
      subroutine ozcot_yield
* ====================================================================

!     estimates yield and gross margin at end of season

      Use Infrastructure
      implicit none

!      real bollsc

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_yield')

*- Implementation Section ----------------------------------
      call push_routine(myname)

!     calculate yield **********************************************************

      g%alint = g%openwt*10.*g%pclint    ! g%g sc/m to kg lint/ha
!psc      alint = alint/rs                   ! adjust for row spacing 2/5/90

!      plntz = uptakn                     ! nitrogen uptake
!pc   bollsc = 0.
!pc   if(openz.gt.0.) bollsc = openwt/openz ! g sc/boll
!jh v2001       g%rrig(rain_pre_post_boll) = g%rrig(rain_preboll)
!jh v2001      :                           + g%rrig(rain_postboll)  ! commulative g%rain pre + post boll

      call ozcot_residues                      ! to calculate stem and root residues
!     calculate gross margin ***************************************************

!     currently redundant!!!!

!      cot_price = 2.                     ! $ per kg
!      wat_cost = 12.                     ! $ per ml pumped
!      spray_cost =2.                     ! $ day of protection

!      cot_price = 2.                      ! cotton price $ per kg
!      gro_cost = 1200.                    ! growing costs - irrigated
!      gro_cost = 450.                     ! growing costs - rain grown
!      wat_cost  = 12.                     ! $ per ml (pumping & carges)
!      spray_cost = 2.                     ! $ per day of protection
!      cot_inc = alint*cot_price           ! cotton income
!      wat_exp = rrig(2)/10.*wat_cost      ! water expenditure
!      spray_save = (150-ilaiz)*spray_cost ! saving of spray cost
!      gross_marg = cot_inc - gro_cost - wat_exp -  spray_save ! gross margin

!     sowing date for output ***************************************************

!      jsow = isow+imet-1                  ! sowing date in j time
!      mdpy_prev = 365
!      if((imyr-1)/4*4 .eq. imyr_1) mdpy_prev = 366
!      if(jsow.gt.mdpy_prev) jsow = jsow - mdpy_prev

!     this section for when crop not sown **************************************

!      if(iend.ge.3) then
!          jsow = 0                        ! crop not sown, window passed or fallow
!          gross_marg = 0.0                ! gross margin nil
!      end if

!     this section does stuff needed with 1st call of yield ********************


!      if (imyr.gt.1800) then              ! year in 10s or 100s?
!          imyr = imyr-1800                ! change year to 10s
!          if(imyr.ge.100) imyr=imyr-100   ! for 1900s
!      endif                               ! above done for output

!      if(nszn.eq.1) then                     ! first season

!          open(3,file='yield.out',status='unknown')
!          write(3,7772) title

!          if(mode.le.1) then                  ! validation or calibration
!              write(3,600)
!          else                                ! simulation for strategy etc
!              if(defirr(2).gt.0.) then        ! irrigated crop
!                  write(3,700)
!              else                            ! rain-fed crop
!                  write(3,800)
!              endif
!          endif

!          if(jdate.lt.244) then               ! last year was year of sowing
!              iyr1 = imyr-1                   ! year sown in first season
!              iyr2 = imyr                     ! year harvested first season
!          else                                ! this year was year of sowing,
!              iyr1 = imyr                     ! year of sowing
!              iyr2 = imyr+1                   ! year of harvest
!          endif

!      else                                    ! not first season

!              iyr1 = iyr1+1                   ! year of sowing of this season
!              iyr2 = iyr2+1                   ! year of harvest of this season


!      endif

!      if(iyr1.eq.100) iyr1 = 0                 ! new century
!      if(iyr2.eq.100) iyr2 = 0                 ! new century

!    fallow soil water *********************************************************


!      if(jsow.gt.0) then    ! crop sown; deal with fallow sw gain
!          gain = rrig(2)-rrig(6) ! gain in sw during fallow
!          fraction = 0.0    ! fraction of rainfall conserved by fallow
!          if(rrig(5).ne.0.) fraction = gain/rrig(5)
!          write(4,999) iyr1,iyr2,jsow,rrig(6),rrig(2),gain,rrig(5),fraction
!999       format(3i4,5f8.2) ! yrs, sow d, initial sw, final sw, gain, rain, fraction
!          rrig(5) = 0.0     ! reset fallow rain. do not reset in reset!
!          rrig(6) = sw      ! initial sw for next fallow. do not reset in reset!
!      endif

!     write output *************************************************************

!      if(mode.le.1) then                  ! validation or calibration
!          write(3,7770) iyr1,iyr2,jsow,openz,alint,bollsc,
!     *    alaiz,plntz,ilaiz,sqzx,isqzx
!          write(2,7770) iyr1,iyr2,jsow,openz,alint,bollsc,
!     *    alaiz,plntz,ilaiz,sqzx,isqzx
!      else                                ! simulation for strategy etc
!          if(defirr(2).gt.0.) then        ! irrigated crop
!              write(3,7771) iyr1,iyr2,jsow,openz,alint,bollsc,
!     *        alaiz,plntz,ilaiz,ifix(rrig(1)),rrig(2),rrig(7),rrig(8)
!     *        alaiz,plntz,ilaiz,ifix(rrig(1)),rrig(2),rrig(7),def_last ! norn d
!              write(2,7771) iyr1,iyr2,jsow,openz,alint,bollsc,
!     *        alaiz,plntz,ilaiz,ifix(rrig(1)),rrig(2),rrig(7),rrig(8)
!     *        alaiz,plntz,ilaiz,ifix(rrig(1)),rrig(2),rrig(7),def_last ! norn d
!          else                            ! rain-fed crop
!              write(3,7771) iyr1,iyr2,jsow,openz,alint,bollsc,
!     *        alaiz,plntz,ilaiz,ifix(rrig(1)),rrig(2),rrig(3),rrig(4)
!              write(2,7771) iyr1,iyr2,jsow,openz,alint,bollsc,
!     *        alaiz,plntz,ilaiz,ifix(rrig(1)),rrig(2),rrig(3),rrig(4)
!          endif
!      endif

      call pop_routine(myname)
      RETURN

!600   format(' year sown  bolls/m  lint sc/boll max_lai  n_uptk day
!     *   sqz day')
!700   format(' year sown  bolls/m  lint sc/boll max_lai  n_uptk day
!     * no  water  rain  cum_et')
!     * no  water  rain def_l')                                    ! for norm d
!800   format(' year sown  bolls/m  lint sc/boll max_lai  n_uptk day
!     * no  water rain1  rain2')
!7770  format(x,2i2,i4,f8.1,f8.0,2f8.2,f8.0,i4,f6.1,i4)
!7771  format(x,2i2,i4,f8.1,f8.0,2f8.2,f8.0,i4,i3,3f7.1)
!7772  format(15a4)

      end subroutine


* ====================================================================
      subroutine ozcot_plant_n
* ====================================================================
!     call from pltgrw before ozcot_cropn
!     calculates nitrogen content of dry matter increments and sums them
!     adjusts n increments as soil n supply diminishes
!     variable ddw_leaf etc from s/r dry_matter
!              supply_n from system
!               dn_plant = daily increment of plant n to system

      Use Infrastructure
      implicit none


      real supply_n
      real conc_l
      real conc_s
      real conc_r
      real conc_b
      real dn_leaf
      real dn_stem
      real dn_root
      real dn_boll
      real sup_dem
      real adjust
      real total_n_pot
      real dn_plant
      real uptakn_max

!jh      data supply_n /2.0/
      data supply_n /1.0/

      DATA CONC_L /0.04/
      DATA CONC_S /0.02/
      DATA CONC_R /0.02/
      DATA CONC_B /0.015/

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_plant_n')

*- Implementation Section ----------------------------------
      call push_routine(myname)

!jhadded
      supply_n = min(2.0, min(g%uptakn/10.0-g%total_n, g%tsno3/10.0)
     :                    /5.0)   ! max uptake of 2 g/m2. 5 days to takeup avail n.
!      supply_n = min(1.0, g%uptakn/240.0)   ! max uptake of 240 kg/ha specified in n_fertilise

!      calculate daily increment for components of dry matter

      dN_LEAF = g%ddw_leaf * CONC_L ! leaf nitrogen
      dN_STEM = g%ddw_stem * CONC_S ! stem nitrogen
      dN_ROOT = g%ddw_root * CONC_R ! root nitrogen
      dN_BOLL = g%ddw_boll * CONC_B ! boll nitrogen

      g%dn_plant = dN_LEAF + dN_STEM + dN_ROOT + dN_BOLL ! plant N increment

!      adjust uptake when soil supply limiting

      if (g%dn_plant.gt.0.) then
        SUP_DEM = SUPPLY_N/g%dn_plant ! supply/demand ratio
      else
        SUP_DEM = 0.
      endif
      ADJUST = ozcot_stress(c%adjust_low
     :                     ,c%adjust_high
     :                     ,c%adjust_a
     :                     ,SUP_DEM) ! factor to adjust
      IF(ADJUST.LT.1.0) THEN
          dN_LEAF = dN_LEAF * ADJUST   ! leaf nitrogen adjusted
          dN_STEM = dN_STEM * ADJUST   ! stem nitrogen adjusted
          dN_ROOT = dN_ROOT * ADJUST   ! root nitrogen adjusted
          dN_BOLL = dN_BOLL * ADJUST   ! boll nitrogen adjusted
          g%dn_plant = dN_LEAF + dN_STEM + dN_ROOT + dN_BOLL ! plant N increment
      ENDIF

      total_n_pot =g%total_n + g%dn_plant ! accumulate uptake for season
      uptakn_max = min(c_uptakn_max, g%uptakn)
!     compare accumulated uptake with projected uptake for season
!     if accumulated exceeds projected, assume requirements met by remobilisation
!     and set this day's increments to zero

!jh      IF(g%total_n.GE.g%uptakn/10.) THEN
!jh          g%total_n =g%total_n - g%dn_plant ! adjust uptake for season
!jh          dN_LEAF = 0.0                ! leaf nitrogen adjusted
!jh          dN_STEM = 0.0                ! stem nitrogen adjusted
!jh          dN_ROOT = 0.0                ! root nitrogen adjusted
!jh          dN_BOLL = 0.0                ! boll nitrogen adjusted
!jh          g%dn_plant = 0.0             ! plant N increment
      IF(total_n_pot.GT.uptakn_max/10.) THEN
          dn_plant = max(0.0, uptakn_max/10. - g%total_n)   ! plant N increment
          adjust = divide (dn_plant, g%dn_plant, 0.0)
          dN_LEAF = dN_LEAF * ADJUST   ! leaf nitrogen adjusted
          dN_STEM = dN_STEM * ADJUST   ! stem nitrogen adjusted
          dN_ROOT = dN_ROOT * ADJUST   ! root nitrogen adjusted
          dN_BOLL = dN_BOLL * ADJUST   ! boll nitrogen adjusted
          g%dn_plant = dn_plant             ! plant N increment
      ENDIF
!      if (g%das .gt. 100
!     :    .and. g%dn_plant.lt. 0.0001
!     :    .and. g%total_n .lt. g%uptakn/10.0)then
!         g%dn_plant = g%uptakn/10.0 - g%total_n
!      if ((g%iend.eq.6 .or. g%iend.eq.10)
!     :    .and. g%total_n .lt. uptakn_max/10.0)then
!         g%dn_plant = uptakn_max/10.0 - g%total_n
!      else
!      endif
!      print*,  g%iend, g%total_n, uptakn_max/10.0

      g%total_n = g%total_n + g%dn_plant ! adjust uptake for season
          g%N_LEAF = dN_LEAF + g%N_LEAF
          g%N_STEM = dN_STEM + g%N_STEM
          g%N_ROOT = dN_ROOT + g%N_ROOT
          g%N_BOLL = dN_BOLL + g%N_BOLL

!      write(4,222) iday,supply_n,dn_plant,total_n,uptakn,dw_total
!222   format(i5,5f8.3)


      call pop_routine(myname)
      RETURN
      end subroutine

* ====================================================================
      subroutine ozcot_residues
* ====================================================================

!      called from s/r yield to calculate stem and root residues

      Use Infrastructure
      implicit none

      real conc_res

      DATA CONC_RES /0.005/           ! N concentration of residues

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_residues')

*- Implementation Section ----------------------------------
      call push_routine(myname)

      g%stem_res = g%dw_stem         ! stem residues dry matter
      g%stem_res_n = g%dw_stem*CONC_RES ! N content of stem residues
      g%root_res = g%dw_root         ! root residues dry matter
      g%root_res_n = g%dw_root*CONC_RES ! N content of root residues

      call pop_routine(myname)
      RETURN
      end subroutine



* ====================================================================
!     subroutine dry_matter (i)
      subroutine ozcot_dryxmatter
* ====================================================================

!     this subroutine is for ozcot6 (version 6).

!     first demand for assimilate is estimated for leaf, stem, root and boll.
!     leaf demand is determined from potential increase in area. water stress
!     may reduce actual area increase, thus giving thicker leaves. inadequate
!     assimilate supply may reduce actal weight increase giving thinner leaves.
!     upper and lower limits are set by leaf weight:area ratio. if the upper
!     limit is exceeded, weight increase is reduced. if lower limit is not
!     reached, area increase is reduced. stem and root demand are determined
!     from potential leaf demand using allometric relationships.

!     calls s/r assimilation to compute assimilate production for the day.
!     the supply available to meet demand is obtained from sum of the days
!     assimilate production and any reserves.

!     supply and demand are then compared in order to partition dry matter
!     increase between leaf, stem, boll and root.
!     if supply exceeds demand, up to two days supply can be stored as reserves.
!     if demand exceeds supply, distribution is simulated on the basis
!     of proximity to the source of supply. leaf, stem and fruit are assumed to
!     be equidistant from the source. if supply exceeds the sum of leaf, stem
!     and fruit demand, their needs are met in full and the balance goes to
!     root. if the sum of leaf, stem and fruit demand exceeds supply,
!     their needs are met in proportion to the supply/demand ratio and the
!     root receives none. the root supply:demand ratio or a decline in root
!     growth provide feedback to reduce increase in root depth in s/r pltgrw.

!     local variables:
!       assimilate new dry matter passed daily from s/r assimilation
!       ddw_boll   day's increase in boll dry weight
!       ddw_leaf   day's increase in leaf dry weight
!       ddw_stem   day's increase in stem dry weight
!       ddw_root   day's increase in root dry weight
!       ddw_root_max   maximum value of increase in root dry weight
!       demand     demand for assimilate for potential day's growth
!       fnstrs2    n stress for boll growth - on/off
!       fwstrs     water stress for boll growth
!       sd_ratio   supply:demand ratio for leaf, stem and boll growth
!       sd_root    supply:demand ratio for root growth
!       strsbl     stress factor for boll growth, minimum of n and water
!       supply     day's supply of assimilate available for potential growth
!       wt_area    leaf weight:area ratio

      Use Infrastructure
      implicit none


      real wt_area
      real fwstrs
      real fnstrs2
      real strsbl
      real assimilate
      real supply
      real demand
      real sd_ratio
      real sd_root
!      real ddw_root_max

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_dryxmatter')

*- Implementation Section ----------------------------------
      call push_routine(myname)

!------------------------------------------------------------------------------
!     initialise leaf, stem and root dry matter at start of new crop
!------------------------------------------------------------------------------

      IF(g%dw_leaf.EQ.0.) THEN              ! leaf weight initialise to zero?
          g%dw_leaf = c%EMBRYO*c%F_LEAF*g%ppm ! initial leaf dry weight per m
          g%dw_stem = c%EMBRYO*c%F_STEM*g%ppm ! initial stem dry weight per m
          g%dw_root = c%EMBRYO*c%F_ROOT*g%ppm ! initial root dry weight per m
      ENDIF

!------------------------------------------------------------------------------
!     calculate demand (potential growth) for leaf, stem and root
!------------------------------------------------------------------------------

      g%ddw_leaf = g%dlai_pot*c%SPECIFIC_LW               ! leaf demand
      g%ddw_stem = divide (c%A_STEM_LEAF*g%ddw_leaf*g%dw_stem
     :                  ,g%dw_leaf, 0.0)                  ! ditto for stem
      g%ddw_root = divide (c%A_ROOT_LEAF*g%ddw_leaf*g%dw_root
     :                  ,g%dw_leaf, 0.0)                  ! ditto for root

!------------------------------------------------------------------------------
!     feed back of leaf weight/area ratio
!------------------------------------------------------------------------------

      IF(g%dlai(g%iday).GT.0.) THEN                       ! leaf growth today?
          WT_AREA = divide (g%ddw_leaf, g%dlai(g%iday), 0.0) ! leaf weight/are ratio
          IF(WT_AREA.GT.c%WT_AREA_MAX) THEN               ! too thick
              g%ddw_leaf = g%dlai(g%iday)*c%WT_AREA_MAX   ! reduce weight
!          else if(wt_area.lt.wt_area_min) then            ! too thin
!              dlai(iday) = ddw_leaf/wt_area_min           ! reduce area
          ENDIF
      ENDIF

!------------------------------------------------------------------------------
!     calculate demand for bolls
!------------------------------------------------------------------------------

!      if(isq.gt.0 .and. i.ge.isq+2) then        ! fruit called yet?
      IF(g%isq.GT.0 .AND. g%das.GE.g%isq+2) THEN  ! FRUIT called yet?             !const
          FWSTRS = ozcot_stress(c%fwstrs_low
     :                           ,c%fwstrs_high
     :                           ,c%fwstrs_a
     :                           ,g%smi)    ! water stress on bolls     !const
          FNSTRS2 = 1.                          ! N stress for bolls off
          IF(g%fnstrs.EQ.0.) FNSTRS2 = 0.       ! N stress for bolls on
          STRSBL = AMIN1(FWSTRS,FNSTRS2)        ! minimum of water or N stress
          g%bollgr = p%scboll*g%bper*p%FBURR ! boll gr rate this g%day
          g%bollgr = g%bollgr*STRSBL            ! adjust for stress
          IF(g%bollgr.LT.0.) g%bollgr = 0.
          g%ddw_boll = g%bollz*g%bollgr         ! boll demand - potential growth

      ENDIF

!------------------------------------------------------------------------------
!   determine supply of assimilate
!------------------------------------------------------------------------------

!      call assimilation(assimilate,i)                  ! day's assimilate
      CALL ozcot_assimilation(ASSIMILATE)                  ! g%day's assimilate
      SUPPLY = ASSIMILATE+g%reserve                    ! compute supply
      g%reserve = 0.                                   ! g%reserve used

!------------------------------------------------------------------------------
!   compare total demand with supply to partition assimilate
!------------------------------------------------------------------------------

      DEMAND = g%ddw_leaf+g%ddw_boll+g%ddw_stem+g%ddw_root ! compute total demand

      IF(SUPPLY.GE.DEMAND) THEN        ! demand met, potential growth achieved
         g%reserve = g%reserve+SUPPLY-DEMAND           ! excess becomes g%reserve
         IF(g%reserve.GT.g%res_cap) g%reserve = g%res_cap ! limit to g%reserve
         SD_RATIO = 1.0                ! supply:demand ratio for leaf,stem,boll          !const
         SD_ROOT = 1.0                 ! supply:demand for root                          !const
      ELSE                             ! demand not met, reduce potential growth
         DEMAND = DEMAND-g%ddw_root    ! demand for leaf, stem and fruit
         IF(SUPPLY.GE.DEMAND) THEN     ! their potential growth achieved
             SD_RATIO = 1.0            ! supply:demand ratio for leaf,stem,boll          !const
             SD_ROOT = divide ((SUPPLY-DEMAND)
     :                         ,g%ddw_root, 0.0)       ! supply:demand for root
             g%ddw_root = SUPPLY-DEMAND ! rest to root
         ELSE                          ! leaf, stem and fruit demand not met
             SD_RATIO = divide (SUPPLY, DEMAND, 0.0)   ! supply:demand ratio
             g%ddw_leaf = g%ddw_leaf*SD_RATIO          ! actual leaf growth
             g%ddw_boll = g%ddw_boll*SD_RATIO          ! actual fruit growth
             g%ddw_stem = g%ddw_stem*SD_RATIO          ! actual stem growth
             g%ddw_root = 0.                           ! no root growth
             SD_ROOT = 0.                              ! supply:demand for root
             g%bollgr = g%bollgr*SD_RATIO              ! adjust boll growth rate
         ENDIF
      ENDIF

!------------------------------------------------------------------------------
!     grow crop by updating dry weights for leaf, stem, bolls and roots
!------------------------------------------------------------------------------

      g%dw_leaf = g%dw_leaf+g%ddw_leaf                 ! total leaf dry weight
      g%dw_stem = g%dw_stem+g%ddw_stem                 ! total stem dry weight
      g%dw_boll = g%dw_boll+g%ddw_boll                 ! total boll dry weight
      g%dw_root = g%dw_root+g%ddw_root                 ! total root dry weight
      g%dw_total = g%dw_leaf+g%dw_stem+g%dw_boll+g%dw_root ! total dry weight

      g%ddw_l(g%iday) = g%ddw_leaf                     ! this g%day's leaf dry wt
!      alai = alai+dlai(iday)                           ! update lai with increase

!------------------------------------------------------------------------------
!     feed back from root grow to root depth
!------------------------------------------------------------------------------
      IF(g%iday.EQ.1) g%ddw_root_max = g%ddw_root        ! initialise max root rate

      IF(g%ddw_root.GT.g%ddw_root_max) THEN
          g%ddw_root_max = g%ddw_root                    ! save maximum root rate
          g%root_feedback = 1.0                        ! feedback of dw on depth
      ELSE
          IF(g%ddw_root_max.EQ.0.) THEN
              g%root_feedback = 1.0
          ELSE
              g%root_feedback = divide (g%ddw_root, g%ddw_root_max, 0.0) ! feedback of dw on depth
          ENDIF
      ENDIF

      g%root_feedback = AMIN1(g%root_feedback,SD_ROOT) ! feedback of dw on depth
      IF(g%root_feedback.GT.0.) g%root_feedback=g%root_feedback**0.333 ! cubic to linear      !const

      g%height = linear_interp_real (
     :                divide(g%dw_stem, g%pp, 0.0),
     :                               p%x_stem_wt,
     :                               p%y_height,
     :                               p%num_height)

!------------------------------------------------------------------------------

      call pop_routine(myname)
      RETURN
      end subroutine



* ====================================================================
!      subroutine assimilation (assimilate,i)
      subroutine ozcot_assimilation (ASSIMILATE)
* ====================================================================

!     assimilate production for the day is estimated from intercepted
!     photosynthetically active radiation (montieth 1977).
!     adjusted for effects of water stress using data of turner et al 1986
!     collected at narrabri ars.
!     effect of water logging based on observation of hearn and constable 1984
!     on yield and hodgson on photosynthesis.
!     effect of temperature, see constables thesis 1981, direct effect on
!     photosynthesis and respiration, using angus and wilson's 1976 expression
!     for a scalar and constables value for base and optimum.
!     carrying capacity, carcap,estimated. it is maximum number of bolls the
!     crop can carry, and is therefore the boll load that causes 100% shedding.
!     local variables:
!       assim_mean      3 day mean of assimilate supply
!       assim_1         previous day's assimilate supply
!       assim_2         day before previous day's supply
!       rad_mj          radiation in mega joules
!       rel_p           relative photosynthesis, scalar for water stress
!       par_int         intercepted photosynthetically active radiation
!       tf              temperature scalar for dry matter production

      Use Infrastructure
      implicit none

!      real assim_1
!      real assim_2
      real rad_mj
      real alight
      real par_int
      real assimilate
      real rel_p
      real tf

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ozcot_assimilation')

*- Implementation Section ----------------------------------
      call push_routine(myname)

!------------------------------------------------------------------------------
!     initialise for new season
!------------------------------------------------------------------------------

!pc   if(iday.eq.1) then
!pc       assim_1 = 0.0                    ! previous day's assimilation
!pc       assim_2 = 0.0                    ! day before that
!psc  endif

!------------------------------------------------------------------------------
!     photosynthesis
!------------------------------------------------------------------------------

      RAD_MJ = g%rad/23.87                 ! langleys to Mjoules                  !const
!      par_int = rad_mj*(1.-tr)*0.5         ! intercepted par

!jh      ALIGHT = 1.-EXP(-1.*g%alai)          ! light interception, Beer's law.
      ! jh changed to the following to accommodate skip row
      g%alai_row = g%alai
      IF(g%NSKIP.GT.0) g%alai_row = g%alai*g%rs         ! lai in hedgerow
      ALIGHT = (1.-EXP(-ozcot_kvalue*g%alai_row)) ! original code  - now gives interception in
      IF(g%NSKIP.GT.0) THEN
         ALIGHT =ALIGHT/g%rs          ! interception on ground area basis
!jh         g%alai = g%alai/g%rs         ! restore LAI to ground area basis
      ENDIF

      PAR_INT = RAD_MJ*(ALIGHT)*0.5        ! intercepted PAR, ex old OZCOT        !const

      ASSIMILATE = PAR_INT*c%e_par         ! assimilation
      IF(ASSIMILATE*2..GT.g%res_cap) THEN
          g%res_cap = ASSIMILATE*2.        ! capacity to store reserves           !const
      ENDIF

!------------------------------------------------------------------------------
!     effect of water stress on assimilation
!------------------------------------------------------------------------------

      REL_P =.25+.864*g%smi                ! effect of water stress on Pp          !const
      IF(REL_P.GT.1.) REL_P = 1.           ! (TURNER g%et al 1986).                !const
      ASSIMILATE = ASSIMILATE*REL_P        ! adjust for water stress

!------------------------------------------------------------------------------
!     effect of temperature on dry matter production
!------------------------------------------------------------------------------

      TF = (g%tempav-c%T_BASE)/(c%T_OPT-c%T_BASE) ! temperature scalar after
      TF = 2*TF-TF**2                      ! Angus & Wilson 1976, Constable 1981     !const
      ASSIMILATE = ASSIMILATE*TF           ! adjust assimilate for temp

!------------------------------------------------------------------------------
!     effect of waterlogging on photosynthesis - hearn & constable 1984 eqn 4
!------------------------------------------------------------------------------

!      if(def.lt.2.5) then                 ! waterlogged?
!jh      IF(g%sw/g%ul.GT.c%watlog_c) THEN           ! waterlogged?                            !const
      IF(g%wli.GT.c%watlog_c) THEN           ! waterlogged?                            !const
          ASSIMILATE = ASSIMILATE*c%wlog_assimilate_red  ! adjust for water logging - old OZCOT    !const
      ENDIF

!jhadded      Effect of N stress
      Assimilate = assimilate*g%vnstrs

!      if(isq.eq.0 .or. i.lt.isq+2) return  ! do not proceed to carrying capacity
      IF(g%isq.EQ.0 .OR. g%das.LT.g%isq+2) then                                       !const
         call pop_routine(myname)
         RETURN ! do not proceed to carrying capacity
      else
      endif

!------------------------------------------------------------------------------
!     carrying capacity - photosynthetic capacity divided by boll growth rate
!------------------------------------------------------------------------------

!      disable rest of subroutine for use with ozcot2 in apsru system

!      if(assim_1.gt.0.0 .and.assim_1.gt.0.0) then      ! not 1st or 2nd crop day
!          assim_mean = (assimilate+assim_1+assim_2)/3. ! 3 day running mean
!      else                                             ! is 1st or 2nd crop day
!          assim_mean = assimilate
!      endif
                                                   ! use mean to buffer g%carcap
!      assim_2 = assim_1                            ! 3rd day's for tomorrow
!      assim_1 = assimilate                         ! 2nd day's for tomorrow

!      if(bollgr.gt.0.0) then
!          carcap_c = assim_mean/bollgr             ! carrying capacity
!      else
!          carcap_c = 0.0                           ! zero when bolls not growing
!      endif

!      if(carcap_c.lt.0.) carcap_c = 0.             ! trap
!      carcap  = carcap_c
!      cutout = carcap*fcutout(ivar)                ! boll load for cutout

!      cutout = carcap*1.00                         ! sensitivity to fcutout

!------------------------------------------------------------------------------

      call pop_routine(myname)
      RETURN
      end subroutine

*     ===========================================================
      subroutine ozcot_read_constants ()
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Purpose
*       Crop initialisation - reads constants from constants file

*+  Changes
*     010994 jngh specified and programmed
*     070495 psc added extra constants (leaf_app etc.)
*     110695 psc added soil temp effects on plant establishment
*     250996 jngh corrected type of lower limit of read_integer_var
*     010998 sb removed year upper and lower bounds.

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'ozcot_read_constants')
*
      character  section_name*(*)
      parameter (section_name = 'constants')

*+  Local Variables
      integer    numvals               ! number of values returned

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call write_string (new_line//'    - Reading constants')

      call read_char_var (section_name
     :                     , 'crop_type', '()'
     :                     , c%crop_type, numvals)

      call read_real_var (section_name
     :                     , 'row_spacing_default', '(m)'
     :                     , c%row_spacing_default, numvals
     :                     , 0.0, 2000.)

      call read_real_var (section_name
     :                     , 'skiprow_default', '()'
     :                     , c%nskip_default, numvals
     :                     , 0.0, 2.0)

!jh      call read_real_var (section_name
!jh     :                     , 'dlds', '()'
!jh     :                     , c%dlds, numvals
!jh     :                     , 0.0, 1.0)

      call read_real_var (section_name
     :                     , 'leaf_res_n_conc', '()'
     :                     , c%leaf_res_n_conc, numvals
     :                     , 0.0, 1.0)


!      call read_real_var (section_name
!     :                     , 'a', '()'
!     :                     , c%a, numvals
!     :                     , 0.0, 1.0)
!
!      call read_real_var (section_name
!     :                     , 'b1', '()'
!     :                     , c%b1, numvals
!     :                     , -1.0, 1.0)
!
!      call read_real_var (section_name
!     :                     , 'b2', '()'
!     :                     , c%b2, numvals
!     :                     , -1.0, 1.0)
!
!      call read_real_var (section_name
!     :                     , 'b3', '()'
!     :                     , c%b3, numvals
!     :                     , 0.0, 1.0)

!jh      call read_integer_var (section_name
!jh     :                     , 'mode', '()'
!jh     :                     , c%mode, numvals
!jh     :                     , -1, 2)

      call read_real_var (section_name
     :                     , 'hucut', '()'
     :                     , c%hucut, numvals
     :                     , 0.0, 100.0)

      call read_real_var (section_name
     :                     , 'baset', '()'
     :                     , c%baset, numvals
     :                     , 0.0, 30.0)

!jh      call read_real_var (section_name
!jh     :                     , 'ambda', '()'
!jh     :                     , c%ambda, numvals
!jh     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'ul1', '()'
     :                     , c%ul1, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'cona', '()'
     :                     , c%cona, numvals
     :                     , 0.0, 1.0)

      call read_real_var (section_name
     :                     , 'open_def', '()'
     :                     , c%open_def, numvals
     :                     , 0.0, 100.0)

!jh      call read_integer_var (section_name
!jh     :                     , 'iwindow', '()'
!jh     :                     , c%iwindow, numvals
!jh     :                     , 0, 100)

!jh      call read_real_var (section_name
!jh     :                     , 'sow_sw', '()'
!jh     :                     , c%sow_sw, numvals
!jh     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'a_root_leaf', '()'
     :                     , c%a_root_leaf, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'a_stem_leaf', '()'
     :                     , c%a_stem_leaf, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'e_par', '(g/mj)'
     :                     , c%e_par, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'specific_lw', '(g/m2)'
     :                     , c%specific_lw, numvals
     :                     , 0.0, 100.0)

      call read_real_var (section_name
     :                     , 't_opt', '(oC)'
     :                     , c%t_opt, numvals
     :                     , 0.0, 50.0)

      call read_real_var (section_name
     :                     , 't_base', '(oC)'
     :                     , c%t_base, numvals
     :                     , 0.0, 20.0)

      call read_real_var (section_name
     :                     , 'wt_area_max', '()'
     :                     , c%wt_area_max, numvals
     :                     , 0.0, 400.0)

!jh      call read_real_var (section_name
!jh     :                     , 'wt_area_min', '()'
!jh     :                     , c%wt_area_min, numvals
!jh     :                     , 0.0, 100.0)

      call read_real_var (section_name
     :                     , 'embryo', '()'
     :                     , c%embryo, numvals
     :                     , 0.0, 1.0)

      call read_real_var (section_name
     :                     , 'f_leaf', '()'
     :                     , c%f_leaf, numvals
     :                     , 0.0, 1.0)

      call read_real_var (section_name
     :                     , 'f_stem', '()'
     :                     , c%f_stem, numvals
     :                     , 0.0, 1.0)

      call read_real_var (section_name
     :                     , 'f_root', '()'
     :                     , c%f_root, numvals
     :                     , 0.0, 1.0)
       call read_real_var (section_name
     :                     , 'elevation_default', '()'
     :                     , c%elevation_default, numvals
     :                     , -100.0, 1000.0)

      call read_real_var (section_name
     :                     , 'wlog_assimilate_red', '()'
     :                     , c%wlog_assimilate_red, numvals
     :                     , 0.0, 1.0)

      call read_real_var (section_name
     :                     , 'wlog_carcap_red', '()'
     :                     , c%wlog_carcap_red, numvals
     :                     , 0.0, 1.0)

      call read_real_var (section_name
     :                     , 'watlog_c', '()'
     :                     , c%watlog_c, numvals
     :                     , 0.0, 1.0)

      call read_real_var (section_name
     :                     , 'watlog_n', '()'
     :                     , c%watlog_n, numvals
     :                     , 0.0, 1.0)

      call read_real_var (section_name
     :                     , 'wlog_carcap_red_stress', '()'
     :                     , c%wlog_carcap_red_stress, numvals
     :                     , 0.0, 1.0)

      call read_real_var (section_name
     :                     , 'smi_affect_wlog', '()'
     :                     , c%smi_affect_wlog, numvals
     :                     , 0.0, 1.0)

      call read_integer_var (section_name
     :                     , 'days_relief_wlog', '(days)'
     :                     , c%days_relief_wlog, numvals
     :                     , 0, 20)

      call read_real_var (section_name
     :                     , 'frost_kill_immediate', '(oC)'
     :                     , c%frost_kill_immediate, numvals
     :                     , -5.0, 5.0)

      call read_real_var (section_name
     :                     , 'rtdep_max', '(cm)'
     :                     , c%rtdep_max, numvals
     :                     , 0.0, 500.0)

      call read_real_var (section_name
     :                     , 'harvest_n_frac', '()'
     :                     , c%harvest_n_frac, numvals
     :                     , 0.0, 1.0)

      call read_real_var (section_name
     :                     , 'cutout_smi_crit', '()'
     :                     , c%cutout_smi_crit, numvals
     :                     , 0.0, 1.0)

      call read_integer_var (section_name
     :                     , 'cutout_smi_days', '()'
     :                     , c%cutout_smi_days, numvals
     :                     , 0, 10)

      call read_real_var (section_name
     :                     , 'cutout_smi_site_red', '()'
     :                     , c%cutout_smi_site_red, numvals
     :                     , 0.0, 1.0)

      call read_real_var (section_name
     :                     , 'epcoef1', '()'
     :                     , c%epcoef1, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'epcoef2', '()'
     :                     , c%epcoef2, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'epcoef_smi_crit', '()'
     :                     , c%epcoef_smi_crit, numvals
     :                     , 0.0, 1.0)

      call read_real_var (section_name
     :                     , 'fbwstr_low', '()'
     :                     , c%fbwstr_low, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'fbwstr_high', '()'
     :                     , c%fbwstr_high, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'fbwstr_a', '()'
     :                     , c%fbwstr_a, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'fbnstr_low', '()'
     :                     , c%fbnstr_low, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'fbnstr_high', '()'
     :                     , c%fbnstr_high, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'fbnstr_a', '()'
     :                     , c%fbnstr_a, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'relp_smi_crit', '()'
     :                     , c%relp_smi_crit, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'relp_intercept', '()'
     :                     , c%relp_intercept, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'relp_slope', '()'
     :                     , c%relp_slope, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'relp_low', '()'
     :                     , c%relp_low, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'relp_high', '()'
     :                     , c%relp_high, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'relp_a', '()'
     :                     , c%relp_a, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'vsnstr_low', '()'
     :                     , c%vsnstr_low, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'vsnstr_high', '()'
     :                     , c%vsnstr_high, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'vsnstr_a', '()'
     :                     , c%vsnstr_a, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'flfsmi_low', '()'
     :                     , c%flfsmi_low, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'flfsmi_high', '()'
     :                     , c%flfsmi_high, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'flfsmi_a', '()'
     :                     , c%flfsmi_a, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'vlnstr_low', '()'
     :                     , c%vlnstr_low, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'vlnstr_high', '()'
     :                     , c%vlnstr_high, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'vlnstr_a', '()'
     :                     , c%vlnstr_a, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'fw_low', '()'
     :                     , c%fw_low, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'fw_high', '()'
     :                     , c%fw_high, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'fw_a', '()'
     :                     , c%fw_a, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'adjust_low', '()'
     :                     , c%adjust_low, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'adjust_high', '()'
     :                     , c%adjust_high, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'adjust_a', '()'
     :                     , c%adjust_a, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'fwstrs_low', '()'
     :                     , c%fwstrs_low, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'fwstrs_high', '()'
     :                     , c%fwstrs_high, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'fwstrs_a', '()'
     :                     , c%fwstrs_a, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'smi_delay_crit', '()'
     :                     , c%smi_delay_crit, numvals
     :                     , 0.0, 10.0)

      call read_real_var (section_name
     :                     , 'cold_shock_delay_crit', '()'
     :                     , c%cold_shock_delay_crit, numvals
     :                     , 0.0, 20.0)

      call read_real_var (section_name
     :                     , 'cold_shock_delay', '()'
     :                     , c%cold_shock_delay, numvals
     :                     , 0.0, 20.0)

      call read_real_var (section_name
     :                     , 'fert_crit', '(kg/ha)'
     :                     , c%fert_crit, numvals
     :                     , 0.0, 100.0)

      call read_real_var (section_name
     :                     , 'fert_detect', '(kg/ha)'
     :                     , c%fert_detect, numvals
     :                     , 0.0, 100.0)

      call read_integer_var (section_name
     :                     , 'days_since_fert_max', '(days)'
     :                     , c%days_since_fert_max, numvals
     :                     , 0, 100)

      call read_real_array (section_name
     :                     , 'x_stem_wt', 20, '(g/plant)'
     :                     , p%x_stem_wt, p%num_height
     :                     , 0.0, 1000.0)

      call read_real_array (section_name
     :                     , 'y_height', 20, '(mm)'
     :                     , p%y_height, p%num_height
     :                     , 0.0, 10000.0)

      call read_real_array_optional (section_name
     :                     , 'x_co2_fert', 20, '()'
     :                     , p%x_co2_fert, p%num_co2_fert
     :                     , 300.0, 1000.0)

      call read_real_array_optional (section_name
     :                     , 'y_co2_fert', 20, '()'
     :                     , p%y_co2_fert, p%num_co2_fert
     :                     , 0.0, 10.0)

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine ozcot_start_crop (variant)
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Purpose
*       Start crop using parameters specified in passed record

*+  Changes
*     010994 jngh specified and programmed
*     090695 psc  add row spacing read
*     220696 jngh changed extract to collect

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'ozcot_start_crop')

*+  Local Variables
      integer, intent(in) :: variant
      type(SowType) :: Sow
      integer    numvals               ! number of values found in array
      character  string*200            ! output string
      real       sdepth_mm             ! sowing depth in mm
      real       row_space_mm          ! row spacing in mm
      real       row_space             ! row spacing in mm
!      character  module_name*8         ! module name

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call Write_string ( 'Sow')

!      call get_current_module (module_name)


              ! get cultivar parameters

      call unpack_Sow(variant, Sow)
              
      sdepth_mm = 0.0
      row_space_mm = 0.0
      g%cultivar = Sow%Cultivar

      call ozcot_read_cultivar_params ()


              ! get other sowing criteria

         ! variety,seed depth,rowspace,plants per m row
         ! cultivar, sowing_depth, row_spacing, plants_pm


      g%ppm_row = Sow%plants_pm
      sdepth_mm = Sow%sowing_depth
      row_space_mm = Sow%row_spacing
      
      if (row_space_mm.eq.0) then
         row_space = c%row_spacing_default
      else
         row_space = row_space_mm /1000.0
      endif

      g%nskip = Sow%SkipRow
      if (g%nskip.lt.0 .or. g%nskip.gt.2) then
         g%nskip = c%nskip_default
      else
      endif

      g%crop_in = .true.
      g%sdepth = sdepth_mm / 10.0
      g%isow = g%jdate
      g%rtdep = g%sdepth
      g%rs = row_space*(2.0 + g%NSKIP)/2.0  !  effective row spacing with skip
      g%ppm_target = g%ppm_row/g%rs    !  adjust for non standard rows incl skip
      g%pp = g%ppm_target*g%rs
      g%ps = (1.0/g%rs)/g%pp
      g%s=g%ps/g%rs
!jh v2001       g%rrig(sw_sowing) = g%sw               ! soil water at sowing
      g%iend = 0
      g%INITIAL = 1
      g%plant_status = status_alive


      call publish_null(id%sowing)

          ! report

      call write_string (new_line//new_line)

      string = '                 Crop Sowing Data'
      call write_string (string)

      string = '    ------------------------------------------------'
      call write_string (string)

      call write_string ('    Sowing  Depth Plants Spacing Cultivar')

      call write_string ('    Day no   mm     m       mm     Name   ')

      string = '    ------------------------------------------------'
      call write_string (string)

      write (string, '(3x, i7, f7.1, f6.1, f9.1, 1x, a10)')
     :                g%isow, sdepth_mm
     :              , g%pp, row_space_mm, g%cultivar
      call write_string (string)

      string = '    ------------------------------------------------'
      call write_string (string)

!      print*, g%crop_in, g%das, g%isow, g%openz, g%iend
      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine ozcot_read_cultivar_params ()
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Purpose
*       Get cultivar parameters for named cultivar, from crop parameter file.

*+  Changes
*       090994 jngh specified and programmed
!
!      20070911 DBJ  added p%BckGndRetn  to allow varietal adjustment for
!                    background retention. (optional parameter)
! ---------------------------------------------------------------------

*+  Calls
                                       ! lu_src_sum

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'ozcot_read_cultivar_params')

*+  Local Variables
      character  string*200            ! output string
      integer    numvals               ! number of values read

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call write_string (new_line//'   - Reading Cultivar Parameters')


      call read_real_var (g%cultivar
     :                    , 'percent_l', '()'
     :                    , p%percent_l, numvals
     :                    , 0.0, 100.0)

      call read_real_var (g%cultivar
     :                    , 'scboll', '()'
     :                    , p%scboll, numvals
     :                    , 0.0, 10.0)

      call read_real_var (g%cultivar
     :                    , 'respcon', '()'
     :                    , p%respcon, numvals
     :                    , 0.0, 1.0)

      call read_real_var (g%cultivar
     :                    , 'sqcon', '()'
     :                    , p%sqcon, numvals
     :                    , 0.0, 1.0)

      call read_real_var (g%cultivar
     :                    , 'fcutout', '()'
     :                    , p%fcutout, numvals
     :                    , 0.0, 1.0)

      call read_real_var (g%cultivar
     :                    , 'flai', '()'
     :                    , p%flai, numvals
     :                    , 0.0, 1.0)

      call read_real_var (g%cultivar
     :                    , 'ddisq', '()'
     :                    , p%DDISQ, numvals
     :                    , 0.0, 1000.0)

!      call read_real_var (g%cultivar
!     :                    , 'TIPOUT', '()'
!     :                    , p%TIPOUT, numvals
!     :                    , 0.0, 100.0)

      call read_real_var (g%cultivar
     :                     , 'popcon', '()'
     :                     , p%POPCON, numvals
     :                     , 0.0, 1.0)

      call read_real_var (g%cultivar
     :                     , 'acotyl', '(mm2)'
     :                     , p%acotyl, numvals
     :                     , 0.0, 1000.0)

      call read_real_var (g%cultivar
     :                     , 'rlai', '()'
     :                     , p%rlai, numvals
     :                     , 0.0, 1.0)

      call read_real_array (g%cultivar
     :                     , 'frudd', max_categories, '(dd)'
     :                     , p%FRUDD, numvals
     :                     , 0.0, 2000.0)

      call read_real_array (g%cultivar
     :                     , 'bltme', max_categories, '()'
     :                     , p%BLTME, numvals
     :                     , 0.0, 1.0)

      call read_real_array (g%cultivar
     :                     , 'wt', max_categories, '()'
     :                     , p%WT, numvals
     :                     , 0.0, 1.0)

      call read_real_var (g%cultivar
     :                     , 'fburr', '()'
     :                     , p%FBURR, numvals
     :                     , 0.0, 5.0)

      call read_real_var (g%cultivar
     :                     , 'dlds_max', '()'
     :                     , p%dlds_max, numvals
     :                     , 0.0, 5.0)

      call read_real_var (g%cultivar
     :                     , 'rate_emergence', '(mm/dd)'
     :                     , p%rate_emergence, numvals
     :                     , 0.0, 10.0)


      call read_real_var_optional (g%cultivar
     :                              , 'BckGndRetn', '()'
     :                              , p%BckGndRetn, numvals
     :                              , 0.0, 1.0)


             ! report

      string = '    ------------------------------------------------'
      call write_string (string)

      write (string, '(4x,2a)')
     :                'Cultivar   = ', g%cultivar
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'percent_L  = '
     :               , p%percent_l
      call write_string (string)

      write (string, '(4x, a, f7.1)')
     :                'scboll     = '
     :               , p%scboll
      call write_string (string)

      write (string, '(4x, a, f7.3)')
     :                'respcon    = '
     :               , p%respcon
      call write_string (string)

      write (string, '(4x, a, f7.3)')
     :                'sqcon      = '
     :               , p%sqcon
      call write_string (string)

      write (string, '(4x, a, f7.4)')
     :                'fcutout    = '
     :               , p%fcutout
      call write_string (string)


      write (string, '(4x, a, f7.1)')
     :                'flai       = '
     :               , p%flai
      call write_string (string)

      write (string, '(4x, a, f7.1)')
     :                'ddisq      = '
     :               , p%DDISQ
      call write_string (string)

!      write (string, '(4x, a, f7.1)')
!     :                'TIPOUT     = '
!     :               , p%TIPOUT
!      call write_string (string)

      write (string, '(4x, a, f7.5)')
     :                'popcon     = '
     :               , p%popcon
      call write_string (string)

      write (string, '(4x, a, f7.0)')
     :                'acotyl     = '
     :               , p%acotyl
      call write_string (string)

      write (string, '(4x, a, f7.5)')
     :                'rlai       = '
     :               , p%rlai
      call write_string (string)

      write (string, '(4x, a, 9f7.0)')
     :                'frudd      = '
     :               , p%frudd
      call write_string (string)

      write (string, '(4x, a, 9f7.2)')
     :                'bltme      = '
     :               , p%bltme
      call write_string (string)

      write (string, '(4x, a, 9f7.4)')
     :                'wt         = '
     :               , p%wt
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'fburr      = '
     :               , p%fburr
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'dLdS_max  = '
     :               , p%dLdS_max
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'rate_emergence  = '
     :               , p%rate_emergence
      call write_string (string)

      IF(p%BckGndRetn.GT.0.0) THEN
      		write (string, '(4x, a, f7.2)')
     :                'Background_Retention = '
     :                , p%BckGndRetn
      ELSE
      		write (string, '(4x, a, f7.2)')
     :                'Background_Retention (default) = '
     :                , 0.8
      ENDIF
      call write_string (string)


      string = '    ------------------------------------------------'
      call write_string (string)

      call write_string (new_line//new_line)

      p%percent_l = p%percent_l / 100.0     ! convert to fraction
      p%rate_emergence = p%rate_emergence/10.0 ! convert from mm to cm
      p%acotyl = p%acotyl/1000000.0 ! convert from mm2 to m2

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine ozcot_read_root_params ()
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Purpose
*       Get root profile parameters

*+  Changes
*       090994 jngh specified and programmed
*     210395 jngh changed from ozcot_section to a parameters section

*+  Calls
                                       ! lu_scr_sum

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'ozcot_read_root_params')
*
      character  section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
      integer    layer                 ! layer number
  !    integer    num_layers            ! number of layers in profile
      character  string*200            ! output string

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call write_string (new_line
     :                  //'   - Reading root profile parameters')

         !       ozcot_sw_supply


      call read_real_array_optional (section_name
     :                     , 'll', max_layers, '(mm/mm)'
     :                     , p%unul, p%num_ll_vals
     :                     , 0.0, 1.0)

      if (p%num_ll_vals.ne.0) then
         ! LL found
      else
         ! LL not found
         call warning_error (err_user
     :         , ' Ozcot LL not found. Using Soilwat LL15 instead.' )
      endif

          ! report
      call write_string (new_line//new_line)

      write (string,'(4x, a)') '    Root Profile'
      call write_string (string)

      string = '------------------------'
      call write_string (string)


      string = '     Layer       Lower '
      call write_string (string)
      string = '     Depth       Limit '
      call write_string (string)

      string = '     (cm)      (mm/mm) '
      call write_string (string)

      string = '------------------------'
      call write_string (string)

      do 2000 layer = 1, p%num_ll_vals
         write (string,'(1x, f9.1,f15.3)')
     :            g%dlayr_cm(layer)
     :          , P%unul(layer)
         call write_string (string)
2000  continue

      string = '------------------------'
      call write_string (string)

      call write_string (new_line//new_line)

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine ozcot_end_crop ()
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Purpose
*       Report occurence of harvest and the current status of specific
*       variables.

*+  Changes
*     010994 jngh specified and programmed
*      191200 dph  changed from unknown_module to all_active_modules
*                  unknown_module not supported in APSIM2.


*+  Calls
                                       ! lu_scr_sum

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'ozcot_end_crop')

*+  Local Variables
       real    res_dm                  ! Residue dry weight (kg/ha)
       real    res_N                   ! Amount of N in residue (kg/ha)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (g%crop_in) then
          ! crop harvested. Report status

         call Write_string ('End crop')

         call ozcot_harvest_report ()
         call ozcot_harvest_update ()
      else
      endif
      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine ozcot_harvest_update ()
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Purpose
*       Report the current status of specific
*       variables.

*+  Changes
*     051101 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'ozcot_harvest_update')

*+  Local Variables
!       real    res_dm                  ! Residue dry weight (kg/ha)
!       real    res_N                   ! Amount of N in residue (kg/ha)

      real       fraction_to_Residue(max_part)   ! fraction sent to residue (0-1)
      real       dlt_dm_crop(max_part)           ! change in dry matter of crop (kg/ha)
      real       dlt_dm_N(max_part)              ! change in N content of dry matter (kg/ha)
      real       root_length(max_layers)

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call publish_null(id%harvesting)

!      res_dm = (g%dw_total - g%openwt / g%rs ) * 10.
!      res_dm = (g%dw_total - g%openwt) * 10.
!      if (res_dm.le.0.) res_dm = 0.
!      res_N = res_dm * 0.4 / 100.0

      fraction_to_residue(:) = 0.0
      dlt_dm_crop(:) = 0.0
      dlt_dm_N(:) = 0.0

      ! Update biomass and N pools.  Different types of plant pools are
      ! ===============================================================
      ! affected differently.
      ! =====================

      dlt_dm_crop(root) = g%dw_root * gm2kg/sm2ha
      dlt_dm_N(root) =  g%n_root * gm2kg/sm2ha
      fraction_to_Residue(root) = 0.0

      dlt_dm_crop(meal) = g%openwt * gm2kg/sm2ha
      dlt_dm_N(meal) =  dlt_dm_crop(meal) * 0.4 / 100.0
      fraction_to_Residue(meal) = 0.0

      dlt_dm_crop(stem) = g%dw_stem * gm2kg/sm2ha
      dlt_dm_N(stem) = g%n_stem * gm2kg/sm2ha
      fraction_to_Residue(stem) = 1.0

      dlt_dm_crop(leaf) = g%dw_leaf * gm2kg/sm2ha
      dlt_dm_N(leaf) = g%n_leaf * gm2kg/sm2ha
      fraction_to_Residue(leaf) = 1.0

      dlt_dm_crop(pod) = (g%dw_boll - g%ddw_boll- g%openwt)*gm2kg/sm2ha
      dlt_dm_crop(pod) = l_bound(dlt_dm_crop(pod), 0.0)
      dlt_dm_N(pod) = g%n_boll * gm2kg/sm2ha - dlt_dm_N(meal)
      dlt_dm_N(pod) = l_bound(dlt_dm_N(pod), 0.0)
      fraction_to_Residue(pod) = 1.0
!     call crop_top_residue (c%crop_type, dm_residue, N_residue)

      if (sum(dlt_dm_crop) .gt. 0.0) then
         call Send_Crop_Chopped_Event
     :             (c%crop_type
     :            , part_name
     :            , dlt_dm_crop
     :            , dlt_dm_N
     :            , fraction_to_Residue
     :            , max_part)
      else
         ! no surface residue
      endif

         call ozcot_root_distrib (root_length
     :                          , dlt_dm_crop(root) * kg2gm/ha2sm)

         call crop_root_incorp (
     .          dlt_dm_crop(root) * kg2gm/ha2sm
     :         ,dlt_dm_N(root) * kg2gm/ha2sm
     :         ,g%dlayr
     :         ,root_length
     :         ,g%rtdep
     :         ,c%crop_type
     :         ,max_layers
     :         ,id%IncorpFOM)

      g%crop_in = .false.
      g%plant_status = status_out
      g%zero_variables = .true.
      g%iend = 0

      g%openwt             = 0.0
      g%total_n             = 0.0
      g%N_BOLL             = 0.0
      g%N_LEAF             = 0.0
      g%N_ROOT             = 0.0
      g%N_STEM             = 0.0
      g%DW_BOLL             = 0.0
      g%DW_LEAF             = 0.0
      g%DW_ROOT             = 0.0
      g%DW_STEM             = 0.0
      g%dDW_BOLL             = 0.0
      g%dDW_LEAF             = 0.0
      g%dDW_ROOT             = 0.0
      g%dDW_STEM             = 0.0
      g%DW_TOTAL            = 0.0
      g%leaf_res = 0.0
      g%leaf_res_n = 0.0
      g%dlai   = 0.0

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine ozcot_update ()
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Purpose
*       Report the current status of specific
*       variables.

*+  Changes
*     051101 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'ozcot_update')

*+  Local Variables
!       real    res_dm                  ! Residue dry weight (kg/ha)
!       real    res_N                   ! Amount of N in residue (kg/ha)

      real       fraction_to_Residue(max_part)   ! fraction sent to residue (0-1)
      real       dlt_dm_crop(max_part)           ! change in dry matter of crop (kg/ha)
      real       dlt_dm_N(max_part)              ! change in N content of dry matter (kg/ha)


*- Implementation Section ----------------------------------

      call push_routine (my_name)

      fraction_to_residue(:) = 0.0
      dlt_dm_crop(:) = 0.0
      dlt_dm_N(:) = 0.0

      ! Update biomass and N pools.  Different types of plant pools are
      ! ===============================================================
      ! affected differently.
      ! =====================

!      dlt_dm_crop(root) = g%dw_root * gm2kg/sm2ha
!      dlt_dm_N(root) =  dlt_dm_crop(root) * 0.4 / 100.0
!      fraction_to_Residue(root) = 1.0
!
!      dlt_dm_crop(meal) = g%openwt * gm2kg/sm2ha
!      dlt_dm_N(meal) =  dlt_dm_crop(meal) * 0.4 / 100.0
!      fraction_to_Residue(meal) = 0.0
!
!      dlt_dm_crop(stem) = g%dw_stem * gm2kg/sm2ha
!      dlt_dm_N(stem) = dlt_dm_crop(stem) * 0.4 / 100.0
!      fraction_to_Residue(stem) = 1.0

      dlt_dm_crop(leaf) = g%leaf_res * gm2kg/sm2ha
      dlt_dm_N(leaf) = g%leaf_res_n * gm2kg/sm2ha
      fraction_to_Residue(leaf) = 1.0

!      dlt_dm_crop(pod) = (g%dw_boll - g%openwt) * gm2kg/sm2ha
!      dlt_dm_crop(pod) = l_bound(dlt_dm_crop(pod), 0.0)
!      dlt_dm_N(pod) = dlt_dm_crop(pod) * 0.4 / 100.0
!      fraction_to_Residue(pod) = 1.0
!     call crop_top_residue (c%crop_type, dm_residue, N_residue)

      if (sum(dlt_dm_crop) .gt. 0.0) then
         call Send_Crop_Chopped_Event
     :             (c%crop_type
     :            , part_name
     :            , dlt_dm_crop
     :            , dlt_dm_N
     :            , fraction_to_Residue
     :            , max_part)
      else
         ! no surface residue
      endif

!      call New_postbox ()
!
!      call post_char_var('dlt_residue_type','()','cotton')
!
!      call post_real_var ('dlt_residue_wt'
!     :                   ,'(kg/ha)'
!     :                   ,res_dm)
!
!      call post_real_var ('dlt_residue_n'
!     :                   ,'(kg/ha)'
!     :                   ,res_N)
!
!      call event_send (unknown_module, 'add_residue')
!
!      call Delete_postbox ()

      g%dDW_BOLL             = 0.0
      g%dDW_LEAF             = 0.0
      g%dDW_ROOT             = 0.0
      g%dDW_STEM             = 0.0

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine ozcot_harvest_report ()
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Purpose
*       Report the current status of specific
*       variables.

*+  Changes
*     051101 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'ozcot_harvest_report')

*+  Local Variables
      character  string*200            ! message
      real       yield                 ! grain yield dry wt (kg/ha)
      real       dm
      real     totnup                   ! N uptake kg/ha
      real     bollsc

*- Implementation Section ----------------------------------

      call push_routine (my_name)

          ! crop harvested. Report status

         dm = g%dw_total * 10.
         totnup = g%total_n * 10.
      if(g%openz.gt.0.0) then
         bollsc = g%openwt/g%openz ! g sc/boll
      else
         bollsc = 0.0
      endif


      call write_string (new_line//new_line)

      write (string, '(a,i6)')
     :            ' Days after sowing      = ', g%das
      call write_string (string)

      write (string, '(a,f6.2,t40,a,f10.1)')
     :            ' bolls/m2               = ',g%openz
     :          , ' Lint (kg/ha)           = ',g%alint
      call write_string (string)

      write (string, '(a,f6.2,t40,a,f10.1)')
     :            ' N uptake (kg/ha)       = ', totnup
     :          , ' bolls sc (g/boll)      = ', bollsc
      call write_string (string)

      write (string, '(a,i6,t40,a,i6)')
     :            ' max squares das (days) = ', g%isqzx
     :          , ' max lai das (days)     = ', g%ilaiz
      call write_string (string)

      write (string, '(a,f6.2,t40,a,f6.3)')
     :            ' maximum squares/m2     = ', g%sqzx
     :          , ' maximum lai (m2/m2)    = ', g%alaiz
      call write_string (string)

      write (string, '(a,f10.1)')
     :            ' total above ground biomass (kg/ha) = ', dm
      call write_string (string)



      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine ozcot_root_distrib (root_array, root_sum)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       root_array(*)         ! (OUTPUT) array to contain
                                       ! distributed material
      real       root_sum              ! (INPUT) Material to be distributed

*+  Purpose
*       Distribute root material over profile

*+  Mission statement
*       Distribute root material over profile

*+  Changes
*       290994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'ozcot_root_distrib')

*+  Local Variables
      real       cum_depth             ! cumulative depth (mm)
      integer    layer                 ! layer number ()
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       root_distrb(max_layers) ! root distribution ()
      real       root_distrb_sum       ! sum of root distribution array

      real       c_root_extinction
      parameter (c_root_extinction = 3.0)

*- Implementation Section ----------------------------------

      call push_routine (my_name)
             ! distribute roots over profile to root_depth

      call fill_real_array (root_array, 0.0, max_layers)
      call fill_real_array (root_distrb, 0.0, max_layers)

      deepest_layer = find_layer_no (g%rtdep, g%dlayr
     :                                , max_layers)
      cum_depth = 0.0
      do 1000 layer = 1, deepest_layer
         cum_depth = cum_depth + g%dlayr(layer)
         cum_depth = u_bound (cum_depth, g%rtdep)
         root_distrb(layer) = exp (-c_root_extinction
     :                      * divide (cum_depth, g%rtdep, 0.0))
1000  continue

      root_distrb_sum = sum_real_array (root_distrb, deepest_layer)
      do 2000 layer = 1, deepest_layer
         root_array(layer) = root_sum * divide (root_distrb(layer)
     :                                        , root_distrb_sum, 0.0)

2000  continue

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Ozcot_ONNew_Met (variant)
*     ===========================================================
      Use Infrastructure
      implicit none

      integer, intent(in) :: variant

*+  Purpose
*     Update met data record

*+  Mission Statement
*     Update met data record

*+  Changes
*        261001 jngh

*+  Local Variables
      integer    numvals
      type(newmetType) :: newmet

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Ozcot_ONNew_Met')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call unpack_newmet(variant, newmet)
      g%solrad = newmet%radn
      g%tempmx = newmet%maxt
      g%tempmn = newmet%mint
      g%rain = newmet%rain

      g%tempav = (g%tempmx + g%tempmn)/2.
      g%solrad = g%solrad / 0.04186            ! convert to langleys
      g%rain = g%rain /10.                     ! convert to cm

      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine ozcot_ONtick (variant)
*     ===========================================================
      Use Infrastructure
      implicit none

      integer, intent(in) :: variant

*+  Purpose
*     Update internal time record and reset daily state variables.

*+  Mission Statement
*     Update internal time record and reset daily state variables.

*+  Changes
*        261001 jngh

*+  Local Variables
      type(timeType) :: tick

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'ozcot_ONtick')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call unpack_time(variant, tick)
      call jday_to_day_of_year(dble(tick%startday), g%jdate, g%imyr)

      call pop_routine (myname)
      return
      end subroutine

!*     ===========================================================
!      subroutine ozcot_ONHail ()
!*     ===========================================================
!      Use Infrastructure
!      implicit none
!
!*+  Purpose
!*     Update internal time record and reset daily state variables.
!
!*+  Mission Statement
!*     Update internal time record and reset daily state variables.
!
!*+  Changes
!*        261001 jngh
!
!*+  Local Variables
!
!*+  Constant Values
!      character*(*) myname               ! name of current procedure
!      parameter (myname = 'ozcot_ONHail')
!
!*- Implementation Section ----------------------------------
!      call push_routine (myname)
!
!      g%Hail = .true.
!
!      call pop_routine (myname)
!      return
!      end subroutine
!

!     RUE modification via CO2
      real function co2FertFX ()
      use infrastructure
      implicit none
      if (p%num_co2_fert .gt. 0) then
        co2FertFX = linear_interp_real (g%co2, p%x_co2_fert,
     :                                  p%y_co2_fert,
     :                                  p%num_co2_fert)
      else
        co2FertFX = 1.0
      endif
      end function

      end module OzcotModule


!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use OzcotModule
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



* ====================================================================
       subroutine Main (action, data_string)
* ====================================================================
      Use Infrastructure
      use OzcotModule
      implicit none
      ml_external Main

*+  Sub-Program Arguments
      character Action*(*)            ! Message action to perform
      character data_string*(*)

*+  Purpose
*      This routine is the interface between the main system and the
*      ozcot module.

*+  Changes
*      psc - 9/08/93
*      DPH - 11/7/94 Modifed routine to zero variables if required
*                    when a process message is received.
*      PdeV  16/3/95 New engine interface
*      jngh  170895  changed manager action to react to sow and harvest actions
*      jngh  250996  added version to presence report
*                    added message_unused call
*      sdb   060599  removed version reference and presence action

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'Ozcot_main')

*- Implementation Section ----------------------------------
      call push_routine(myname)

      if (Action.eq.ACTION_Get_variable) then
         call ozcot_Send_my_variable (data_string)

      else if (Action .eq. ACTION_Set_variable) then
         call ozcot_set_my_variable (data_string)

      else if (Action .eq. ACTION_prepare) then
         call ozcot_prepare ()

!      elseif (action.eq.EVENT_Hail) then
!         call ozcot_ONHail ()

      else if (Action.eq.ACTION_Process) then
         if (g%zero_variables) then
            call ozcot_zero_variables()
            call ozcot_init()

         else
            ! No need to zero variables.
         endif

         call ozcot_get_other_variables ()
         call ozcot_Process ()
         call ozcot_set_other_variables ()

      else if (Action .eq. ACTION_Post) then
         call ozcot_post ()

!jh      else if (Action .eq. 'sow' .or. action .eq. 'harvest') then
!jh         call ozcot_manager (Action, data_string)

      elseif (action.eq.ACTION_end_crop) then
         ! end crop - turn into residue
         if (g%crop_in) then
            call ozcot_end_crop ()
         endif

      elseif (action.eq.ACTION_kill_crop) then
               ! kill crop - die
!            call ozcot_kill_crop
!     :               (
!     :                g%dm_dead
!     :              , g%dm_green
!     :              , g%dm_senesced
!     :              , g%plant_status
!     :               )
      else if (Action .eq. ACTION_End_run) then
         call ozcot_end_run ()

      elseif (Action.eq.ACTION_init) then
         call ozcot_zero_all_globals()
         call ozcot_zero_variables ()
         call ozcot_get_other_variables ()
         call ozcot_Init ()

      else
         ! Don't use message
         call message_unused ()

      endif

      call pop_routine(myname)
      return
      end subroutine

* ====================================================================
      subroutine doInit1()
* ====================================================================
      use OzcotModule
      Use infrastructure
      implicit none
      ml_external doInit1
!STDCALL(doInit1)
      integer dummy

      id%crop_chopped = add_registration(eventReg, 'crop_chopped',
     :                                   ApsimVariantTypeDDML, '')
      id%sowing = add_registration(eventReg, 'sowing',
     :                             nullTypeDDML, '')
      id%harvesting = add_registration(eventReg, 'harvesting',
     :                                 nullTypeDDML, '')

      id%create = add_registration(respondToEventReg, 'create',
     :                             nullTypeDDML, '')
      id%sysinit = add_registration(respondToEventReg, 'sysinit',
     :                              nullTypeDDML, '')
      id%end_run = add_registration(respondToEventReg, 'end_run',
     :                              nullTypeDDML, '')
      id%sow = add_registration(respondToEventReg, 'sow',
     :                          sowTypeDDML, '')
      id%harvest = add_registration(respondToEventReg, 'harvest',
     :                              nullTypeDDML, '')
      id%end_crop = add_registration(respondToEventReg, 'end_crop',
     :                               nullTypeDDML, '')
      id%kill_crop = add_registration(respondToEventReg, 'kill_crop',
     :                                nullTypeDDML, '')
      id%tick = add_registration(respondToEventReg, 'tick',
     :                           nullTypeDDML, '')
      id%newmet = add_registration(respondToEventReg, 'newmet',
     :                             newmetTypeDDML, '')
      id%prepare = add_registration(respondToEventReg, 'prepare',
     :                              nullTypeDDML, '')
      id%process = add_registration(respondToEventReg, 'process',
     :                              nullTypeDDML, '')
      id%post = add_registration(respondToEventReg, 'post',
     :                           nullTypeDDML, '')
      id%IncorpFOM = add_registration(eventReg, 'IncorpFOM',
     :                                FOMLayerTypeDDML, '')

      dummy = add_registration_with_units(getVariableReg, 'dlayer',
     :                                    floatarrayTypeDDML, 'mm')
      dummy = add_registration_with_units(getVariableReg, 'bd',
     :                                    floatarrayTypeDDML, 'g/cm3')
      dummy = add_registration_with_units(getVariableReg, 'll15',
     :                                    floatarrayTypeDDML, 'mm/mm')
      dummy = add_registration_with_units(getVariableReg, 'dul',
     :                                    floatarrayTypeDDML, 'mm/mm')
      dummy = add_registration_with_units(getVariableReg, 'sat',
     :                                    floatarrayTypeDDML, 'mm/mm')
      dummy = add_registration_with_units(getVariableReg, 'es',
     :                                    floatTypeDDML, 'mm')
      dummy = add_registration_with_units(getVariableReg, 'runoff',
     :                                    floatTypeDDML, 'mm')
      dummy = add_registration_with_units(getVariableReg, 'sw',
     :                                    floatarrayTypeDDML, 'mm/mm')
      dummy = add_registration_with_units(getVariableReg, 'no3_min',
     :                                    floatarrayTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(getVariableReg, 'no3',
     :                                    floatarrayTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(getVariableReg, 'nh4_min',
     :                                    floatarrayTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(getVariableReg, 'nh4',
     :                                    floatarrayTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(getVariableReg, 'urea',
     :                                    floatarrayTypeDDML, 'kg/ha')

      dummy = add_registration_with_units(respondToGetReg, 'das',
     :                                    intTypeDDML, 'days')
      dummy = add_registration_with_units(respondToGetReg, 'crop_type',
     :                                    stringTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'sumdd',
     :                                    floatTypeDDML, 'oCd')
      dummy = add_registration_with_units(respondToGetReg, 'sites',
     :                                    floatTypeDDML, '1/m2')
      dummy = add_registration_with_units(respondToGetReg, 'squarz',
     :                                    floatTypeDDML, '1/m2')
      dummy = add_registration_with_units(respondToGetReg, 'fru_no_cat',
     :                                    floatarrayTypeDDML, '1/m2')
      dummy = add_registration_with_units(respondToGetReg, 'bollz',
     :                                    floatTypeDDML, '1/m2')
      dummy = add_registration_with_units(respondToGetReg, 'openz',
     :                                    floatTypeDDML, '1/m2')
      dummy = add_registration_with_units(respondToGetReg, 'alint',
     :                                    floatTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(respondToGetReg, 'openwt',
     :                                    floatTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(respondToGetReg, 'frudw',
     :                                    floatTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(respondToGetReg, 'frudw_tot',
     :                                    floatTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(respondToGetReg, 'frudw_shed',
     :                                    floatTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(respondToGetReg, 'frun',
     :                                    floatTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(respondToGetReg, 'bload',
     :                                    floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'carcap_c',
     :                                    floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'carcap_n',
     :                                    floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'vnstrs',
     :                                    floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'fnstrs',
     :                                    floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'dm',
     :                                    floatTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(respondToGetReg, 'dw_boll',
     :                                    floatTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(respondToGetReg, 'dw_root',
     :                                    floatTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(respondToGetReg, 'dw_leaf',
     :                                    floatTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(respondToGetReg, 'dw_stem',
     :                                    floatTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(respondToGetReg, 'totnup',
     :                                    floatTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(respondToGetReg, 'yield',
     :                                    floatTypeDDML, 'bales/ha')
      dummy = add_registration_with_units(respondToGetReg, 'lint_yield',
     :                                    floatTypeDDML, 'bales/ha')
      dummy = add_registration_with_units(respondToGetReg, 'lai',
     :                                    floatTypeDDML, 'm^2/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                               'cover_green', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'cover_tot',
     :                                    floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'height',
     :                                    floatTypeDDML, 'mm')
      dummy = add_registration_with_units(respondToGetReg, 'availn',
     :                                    floatTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(respondToGetReg, 'uptakn',
     :                                    floatTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(respondToGetReg, 'tsno3',
     :                                    floatTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(respondToGetReg, 'ysno3',
     :                                    floatTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(respondToGetReg, 'tsnh4',
     :                                    floatTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(respondToGetReg, 'ysnh4',
     :                                    floatTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(respondToGetReg, 'n_uptake',
     :                                    floatTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(respondToGetReg, 'rtdep',
     :                                    floatTypeDDML, 'cm')
      dummy = add_registration_with_units(respondToGetReg, 's_bed_mi',
     :                                    floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'smi',
     :                                    floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'wli',
     :                                    floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'evap_plant',
     :                                    floatTypeDDML, 'cm')
      dummy = add_registration_with_units(respondToGetReg, 'evap_soil',
     :                                    floatTypeDDML, 'cm')
      dummy = add_registration_with_units(respondToGetReg, 'evap_pot',
     :                                    floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                            'ozcot_crop_in',booleanTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                           'ozcot_status', intTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'bolls_sc',
     :                                    floatTypeDDML, 'g/boll')
      dummy = add_registration_with_units(respondToGetReg, 'squarz_max',
     :                                    floatTypeDDML, '1/m2')
      dummy = add_registration_with_units(respondToGetReg, 'lai_max',
     :                                    floatTypeDDML, 'm2/m2')
      dummy = add_registration_with_units(respondToGetReg, 'i_def',
     :                                    intTypeDDML, 'das')
      dummy = add_registration_with_units(respondToGetReg, 'i_def2',
     :                                    intTypeDDML, 'das')

      call ozcot_zero_all_globals ()
      end subroutine doInit1

! ====================================================================
! This routine is the event handler for all events
! ====================================================================
      subroutine respondToEvent(fromID, eventID, variant)
      use OzcotModule
      Use infrastructure
      implicit none
      ml_external respondToEvent
!STDCALL(respondToEvent)

      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant

      if (eventID .eq. id%tick) then
         call Ozcot_ONtick(variant)
      else if (eventID .eq. id%newmet) then
         call Ozcot_ONNew_Met(variant)
      elseif (eventID .eq. id%sow) then
         ! request and receive variables from owner-modules
            if (g%zero_variables) then
               call ozcot_zero_variables()
               call ozcot_init()

            else
               ! No need to zero variables.
            endif
               ! start crop and do  more initialisations
            call ozcot_start_crop (variant)

      elseif (eventID .eq. id%harvest) then
         ! harvest crop - turn into residue
         if (g%crop_in) then
            call ozcot_manager('harvest', ' ')
         endif
      endif
      return
      end subroutine respondToEvent

