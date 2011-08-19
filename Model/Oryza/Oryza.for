      module oryzaModule
      use registrations
      use infrastructure
      Use CropLibrary
 
! ===================================================================
!     oryza constants
! ===================================================================

!     Global variables

      Integer max_irri            ! array size
      Parameter (max_irri = 200)

      integer max_layer           ! array size
      parameter (max_layer = 100)

      integer max_solute          ! array size
      parameter (max_solute = 10)

      integer max_table           ! array size
      parameter (max_table = 10)

      REAL   Tiny                 ! Parameter with very small value (to avoid division by 0)
      parameter (tiny = 1.0E-5)

      real       svp_A
      parameter (svp_A = 6.106)   ! Teten coefficients

      real       svp_B
      parameter (svp_B = 17.27)   ! Teten coefficients

      real       svp_C
      parameter (svp_C = 237.3)   ! Teten coefficients

      real svp_fract              !weights vpd towards vpd at maximum temperature
      parameter (svp_fract =  0.66)

      character  status_alive*(*) ! crop status
      parameter (status_alive = 'alive')

      character  status_dead*(*)
      parameter (status_dead = 'dead')

      character  status_out*(*)
      parameter (status_out = 'out')

      ! Water balance states
      integer    env_potential        ! no water balance plugged in
      parameter  (env_potential = 1)

      integer    env_limited          ! water balance is available
      parameter  (env_limited = 2)

      integer    ET_LINEAR            ! Et methods
      parameter  (ET_LINEAR = 1)

      integer    ET_EXPONENTIAL
      parameter  (ET_EXPONENTIAL = 2)

      type oryzaGlobals
         sequence
         character cultivar*20     ! name of cultivar
         real no3(max_layer)       ! amount of NO3 in each soil layer (kg/ha)
         real nh4(max_layer)       ! amount of NH4 in each soil layer (kg/ha)
         REAL MSKPA(max_layer)     ! Array with soil water potential/layer (KPa)
         REAL WCL(max_layer)       ! Array of actual soil water content, per soil layer (m3/m3)
         Real Fact(max_layer)      ! Soil-water tension (pF)
         real Fvpd                 ! Interpolated VPD stress factor (0-1)
         REAL eff                  ! Initial light-use efficiency (kg CO2 ha-1 leaf h-1)(W m-2 leaf )-1
         REAL MSUC(max_layer)      ! Array of soil-water tension (suction), per soil layer  !cm H2O
         REAL TRWL(max_layer)      ! Array of actual water withdrawal by transpiration, per soil layer  !mm d-1
         REAL ZRTL(max_layer)      ! Array of root length in a soil layer, per soil layer  !m
         REAL root_weight_layer(max_layer) !  Array of root mass in a soil layer (kg/ha)
         REAL root_length_layer(max_layer) !  Array of root length in a soil layer (mm/mm2)
         REAL rlv(max_layer)               !  Array of root length per unit volume for each soil layer (mm/mm3)
         real tmmx                 ! Daily maximum temperature (degrees C)
         REAL tmmn                 ! Daily minimum temperature (degrees C)
         REAL tmda                 ! Daily average temperature (degrees C)
         REAL ZRT                  ! Root length or rooting depth  !m
         REAL RLAI                 ! Rice leaf area index
         real max_rlai             ! maximum rice_lai achieved
         Real ETD                  ! Reference evapotranspiration (mm d-1)
         REAL ETRD                 ! Radiation-driven part of reference evapotranspiration rate  !mm d-1
         REAL ETAE                 ! Dryness-driven part of reference evapotranspiration rate  !mm d-1
         REAL TRC                  ! Potential transpiration rate of crop with given LAI  !mm d-1
         REAL WLVG                 ! Dry weight of green leaves  !kg ha-1
         REAL WSTS                 ! Dry weight of structural stems  !kg ha-1
         REAL WST                  ! Dry weight of stems  !kg ha-1
         REAL WSTR                 ! Dry weight of stem reserves  !kg ha-1
         REAL WLV                  ! Dry weight of leaves  !kg ha-1
         REAL WLVD                 ! Dry weight of dead leaves  !kg ha-1
         REAL WAG                  ! Total aboveground green dry matter  !kg DM ha-1
         REAL WSO                  ! Dry weight of storage organs  !kg ha-1
         REAL WAGT                 ! Total aboveground dry matter  !kg DM ha-1
         REAL TDRW                 ! Total aboveground and belowground dry biomass  !kg ha-1
         REAL WRT                  ! Dry weight of roots  !kg ha-1
         REAL DVS                  ! Development stage of crop   !-
         REAL LESTRS               ! Drought stress factor reducing leaf expansion  !-
         REAL TMPCOV               ! Temperature increase caused by greenhouse use (over seedbed)  !°C
         REAL TAV                  ! Average daily temperature   !°C
         REAL TAVD                 ! Average daytime temperature  !°C
         REAL DTR                  ! Daily total global radiation  !J m-2 d-1
         REAL TMAX                 ! Daily maximum temperature   !°C
         REAL TMIN                 ! Daily minimum temperature   !°C
         REAL hu                   ! Daily heat units effective for phenological development  !°Cd d-1
         REAL hulv                 ! Daily heat units effective for leaf area development  !°Cd d-1
         REAL DVR                  ! Development rate of crop   !°Cd-1
         REAL DAYL                 ! Astronomic daylength (base = 0 degrees)   !h
         REAL TS                   ! Temperature sum for phenological development  !°Cd
         REAL TSHCKD               ! Transplanting shock for phenological development   !°Cd
         REAL LRSTRS               ! Drought stress factor causing leaf rolling  !-
         REAL LAIROL               ! Rolled leaf area index caused by drought  !ha leaf ha-1 soil
         REAL SSGA                 ! Specific green stem area  !ha stem kg-1 stem
         REAL ARLAI                ! Apparent leaf area index (including stem area)  !ha leaf ha-1 soil
         REAL SAI                  ! Stem area index  !ha leaf ha-1 soil
         REAL KDF                  ! Extinction coefficient for leaves  !-
         REAL REDF                 ! Index of effect of temperature on AMAX
         REAL KNF                  ! Extinction coefficient of nitrogen profile in canopy  !-
         REAL NFLV                 ! Nitrogen fraction in leaves on leaf area basis  !g N m-2 leaf
         REAL cslv                 ! Scattering coefficient of leaves for photosynthetically active radiation  !-
         REAL ecpdf                ! Extinction coefficient for diffuse radiation  !-
         REAL gai                  ! Green area index   !ha leaf ha-1 soil
         REAL gpcdt                ! Daily total gross assimilation   !kg CO2 ha-1 d-1
         REAL dtga                 ! Daily total gross CO2 assimilation of crop  !kg CO2 ha-1 d-1
         REAL lat                  ! Latitude of weather station   !dec. degr.
         REAL SINLD                ! Intermediate variable in calculating solar declination  !-
         REAL COSLD                ! Intermediate variable in calculating solar height  !-
         REAL DAYLP                ! Photoperiodic astronomic daylength (base = -4 degrees)  !h
         REAL DSINB                ! Daily total of sine of solar height   !s d-1
         REAL DSINBE               ! As DSINB, but with a correction for lower atmospheric transmission at lower solar elevations  !s d-1
         REAL SOLCON               ! Solar constant at day = IDOY   !W m-2
         REAL ANGOT                ! Daily extraterrestrial radiation   !J m-2 d-1
         REAL RAPCDT               ! Daily rate of absorbed photosynthetically active radiation  !J m-2  d-1
         REAL hour                 ! Hour of day at which solar height is maximum  !h
         REAL RDPDF                ! Instantaneous flux of diffuse photosynthetically active radiation  !W m-2
         REAL RDPDR                ! Instantaneous flux of direct photosynthetically active radiation  !W m-2
         REAL SINB                 ! Sine of solar height   !-
         REAL GPC                  ! Instantaneous CO2 assimilation rate of canopy   !kg CO2 ha-1 h-1
         REAL RAPC                 ! Instantaneous absorbed photosynthetically active radiation  !W m-2 leaf
         REAL GAID                 ! Green area index above selected height   !ha leaf ha-1 soil
         REAL AMAX1                ! Uncorrected CO2 assimilation rate at light saturation   !kg CO2 ha-1 leaf h-1
         REAL EFF1                 ! Uncorrected initial light-use efficiency  !(kg CO2 ha-1 leaf h-1)(W m-2 leaf )-1
         REAL RAPSHL               ! Absorbed flux for shaded leaves  !W m-2 leaf
         REAL RAPPPL               ! Direct flux absorbed by leaves perpendicular on direct beam  !W m-2 leaf
         REAL FSLLA                ! Fraction of leaf area that is sunlit  !-
         REAL AMAX2                ! Corrected CO2 assimilation rate at light saturation  !kg CO2 ha-1 leaf h-1
         REAL EFF2                 ! Corrected initial light-use efficiency  !(kg CO2 ha-1 leaf
         REAL GPSHL                ! Gross CO2 assimilation rate of shaded leaves  !kg CO2 ha-1 leaf h-1
         REAL GPSLL                ! Gross CO2 assimilation rate of sunlit leaves  !kg CO2 ha-1 leaf h-1
         REAL RAPSLL               ! Total absorbed radiation  !W m-2 leaf
         REAL GPL                  ! Instantaneous CO2 assimilation rate of leaves at depth GAI  !kg CO2 ha-1 leaf h-1
         REAL RAPL                 ! Absorbed photosynthetically active radiation in canopy  !W m-2 soil
         REAL PARI1                ! Amount of photosynthetically active radiation absorbed on a day by canopy  !MJ m-2 d-1
         REAL DPARI                ! The amount of photosynthetically active radiation that is absorbed in a day by canopy  !MJ m-2 d-1
         REAL DPAR                 ! Daily incoming photosynthetically active radiation  !MJ m-2 d-1
         REAL PCEW                 ! Effect of drought stress on daily total gross CO2 assimilation of crop; reduction in potential transpiration rate  !-
         REAL FSH                  ! Fraction of total dry matter allocated to shoots  !-
         REAL FLV                  ! Fraction of shoot dry matter allocated to leaves  !-
         REAL FST                  ! Fraction of shoot dry matter allocated to stems  !-
         REAL FSO                  ! Fraction of shoot dry matter allocated to storage organs  !-
         REAL CPEW                 ! Stress factor for assimilate partitioning (-)
         REAL FRT                  ! Fraction of total dry matter allocated to roots  !-
         REAL GGR                  ! Rate of increase in grain weight  !kg DM ha-1 d-1
         REAL PWRR                 ! Potential weight of rough rice  !kg ha-1
         REAL WRR                  ! Dry weight of rough rice (final yield)  !kg ha-1
         REAL GCR                  ! Gross growth rate of crop  !kg DM ha-1 d-1
         REAL DRLV                 ! leaf death coefficient
         REAL NSLLV                ! N stress factor that accelerates leaf death  !-
         REAL LLV                  ! Loss rate of leaf weight  !kg DM ha-1 d-1
         REAL NGCR                 ! Net growth rate of crop, including translocation  !kg ha-1 d-1
         REAL PLTR                 ! Intermediate variable for change in plant density at transplanting  !-
         REAL GRT                  ! Growth rate of roots  !kg DM ha-1 d-1
         REAL GLV                  ! Growth rate of leaves  !kg ha-1 d-1
         REAL RWLVG                ! Growth rate of green leaves  !kg ha-1 d-1
         REAL GST                  ! Growth rate of stems  !kg DM ha-1 d-1
         REAL GSTR                 ! Growth rate of stem reserves  !kg DM ha-1 d-1
         REAL RWSTR                ! Net growth rate of stem reserves  !kg ha-1 d-1
         REAL GSO                  ! Growth rate of storage organs  !kg DM ha-1 d-1
         REAL SF1                  ! Spikelet sterility factor because of low temperatures   !-
         REAL SF2                  ! Spikelet fertility factor because of high temperatures   !-
         REAL SPFERT               ! Spikelet fertility factor  !-
         REAL GNSP                 ! Rate of increase in spikelet number   !no. ha-1 d-1
         REAL GNGR                 ! Rate of increase in grain number   !no. ha-1 d-1
         REAL NSP                  ! Number of spikelets   !no. ha-1
         REAL sla                  ! Specific leaf area   !ha leaf kg-1 leaf
         REAL RGRL                 ! Relative growth rate for leaf development   !°Cd-1
         REAL RNSTRS               ! Reduction factor on relative leaf growth rate caused by N stress  !-
         REAL GLAI                 ! Growth rate of leaf area index   !ha ha-1 d-1
         REAL TSLV                 ! Temperature sum for leaf area development  !°Cd
         REAL DLDR                 ! Death rate of leaves caused by drought  !kg DM ha-1 d-1
         REAL DLDRT                ! Total death rate of leaves caused by drought  !kg DM ha-1 d-1
         REAL LDSTRS               ! Drought stress factor accelerating leaf death   !-
         REAL CKCIN                ! Carbon in crop accumulated since simulation started  !kg C ha-1
         REAL CKCFL                ! Sum of integrated carbon fluxes into and out of crop  !kg C ha-1
         REAL TNASS                ! Total net CO2 assimilation  !kg CO2 ha-1
         REAL RTNASS               ! Net rate of total CO2 assimilation by crop  !kg CO2 ha-1 d-1
         REAL PARCUM               ! Cumulative amount of radiation absorbed by canopy based on detailed calculation of daily absorbed radiation  !MJ m-2
         REAL PARCM1               ! Cumulative amount of radiation absorbed by canopy based on the simple calculation of daily absorbed radiation  !MJ m-2
         REAL NGR                  ! Number of grains  !no ha-1
         REAL NGRM2                ! Number of hills   !hills m-2
         REAL NSPM2                ! Number of spikelets  !no. m-2
         REAL ZRTM                 ! Maximum root length/depth  !m
         REAL TKLT                 ! Thickness of combined soil layers   !m
         REAL RAIN                 ! Daily amount of rainfall   !mm d-1
         REAL TRW                  ! Actual transpiration rate of crop with given LAI  !mm d-1
         REAL LRAV                 ! Drought stress factor causing leaf rolling, mean over all soil layers  !-
         REAL LDAV                 ! Drought factor accelerating leaf death, mean over all soil layers  !-
         REAL LEAV                 ! Drought stress factor reducing leaf expansion, mean over all soil layers  !-
         REAL ZLL                  ! Summed root depths of preceding soil layers   !m
         REAL TRRM                 ! Potential transpiration rate of crop with given LAI, per unit root length  !mm d-1 m-1
         REAL tnsoil               ! Daily amount of N available for uptake from soil  !kg N ha-1 d-1
         REAL NACR                 ! Actual nitrogen uptake rate by crop  !kg N ha-1 d-1
         REAL FNLV                 ! Fraction of N in leaves on weight basis  !kg N kg-1 DM
         REAL FNST                 ! Fraction of N in stems  !kg N kg-1 DM
         REAL NMAXL                ! Maximum N fraction in leaves on weight basis  !kg N kg-1 DM
         REAL NMINL                ! Minimum N fraction in leaves on weight basis  !kg N kg-1 DM
         REAL NMINSO               ! Minimum N fraction in storage organs  !kg N kg-1 DM
         REAL ANCRF                ! Amount of N in crop till flowering  !kg N ha-1
         REAL NLV                  ! Daily net flow rate of N to the leaves  !kg N ha-1 d-1
         REAL NST                  ! Net flow rate of N to stems  !kg N ha-1 d-1
         REAL NSO                  ! Net flow rate of N to storage organs  !kg N ha-1 d-1
         REAL FNSO                 ! Fraction of N in storage organs  !kg N kg-1 DM
         REAL anlv                 ! Amount of N in leaves  !kg N ha-1
         REAL anld                 ! Amount of N in dead leaves  !kg N ha-1
         REAL anst                 ! Amount of N in stems  !kg N ha-1
         REAL ANCR                 ! Amount of N in crop (live and dead material)  !kg N ha-1
         REAL anso                 ! Amount of N in storage organs  !kg N ha-1
         REAL ANSTA                ! Amount of N in stems till flowering  !kg N ha-1
         REAL ANLVA                ! Amount of N in leaves till flowering  !kg N ha-1
         REAL NALVS                ! Cumulative amount of nitrogen taken up by leaves  !kg N ha-1
         REAL NASTS                ! Cumulative amount of nitrogen taken up by stems  !kg N ha-1
         REAL NASOS                ! Cumulative amount of nitrogen taken up by storage organs  !kg N ha-1
         REAL NACRS                ! Cumulative amount of nitrogen taken up by crop  !kg N ha-1
         REAL NTRTS                ! Amount of N translocated from roots to storage organs  !kg N ha-1
         real COLDTT               ! Accumulated cold degree days (degree days)
         real TFERT                ! Average daily maximum temperature during flowering  !°C
         REAL NTFERT               ! Number of days of flowering period  !d
         REAL TSLVTR               ! Temperature sum for leaf area development at transplanting  !°Cd
         REAL TSHCKL               ! Transplanting shock for leaf area development  !°Cd
         REAL X                    ! Intermediate variable for numerical integration  !-
         REAL TESTSET              ! Maximum difference between simulated and user-supplied SLA  !ha leaf kg-1 leaf
         REAL WLVGEXP              ! Value of WLVG at end of exponential growth phase after transplanting  !kg ha-1
         REAL LAIEXP               ! Value of LAI at end of exponential growth phase after transplanting  !ha leaf ha-1 soil
         REAL WLVGEXS              ! Value of WLVG at end of exponential growth phase in seedbed  !kg ha-1
         REAL LAIEXS               ! Value of LAI at end of exponential growth phase in seedbed  !ha leaf ha-1 soil
         REAL TEST                 ! Difference between simulated and user-supplied SLA  !ha leaf kg-1 leaf
         REAL CO2                   ! Ambient CO2 concentration  !ppm
         ! dsg 030809  Rat grazing work with peter brown
         real rat_graze_perc        ! daily rat-grazing proportion of crop (if vegetative applied to g%WLVG (dry weight of leaves) 
                                    !                                     if reproductive applied to g%WSO (weight of storage organs))
         real gr_rat_grazed         !  grain weight grazed by rats on daily basis (kg/ha/d)
         REAL llv_rat               !  Loss rate of leaf weightdue to rat grazing !kg ha-1 d-1 

         character plant_status*5  ! status of crop

         INTEGER Cropsta           ! Crop stage   !-
         Integer NCOLD             ! Number of cold days   !d
         INTEGER DAE               ! Days after emergence  !d
         Integer IDOY              ! Day number within year of simulation   !d

         LOGICAL GRAINS            ! Logical parameter indicating whether grains are formed   !-
         LOGICAL TESTL             !Logical variable to indicate whether the difference between simulated and imposed SLA is smaller than TESTSET  !-
         character*50 eo_source    ! system variable name of external eo source
      end type oryzaGlobals
! ===================================================================
      type oryzaParameters
         sequence
         character uptake_source*32 ! 'apsim' = swim, calc = us.
         character crop_type*32     ! Low-land rice or high-land rice
         character estab*32         ! Method of crop establishment  !-
         integer  swirtr            ! Switch to select calculation mode for relative transpiration ratio  !-
         integer  prodenv           ! Name of production environment with respect to water   !-
         integer  nitroenv          ! Name of production environment with respect to nitrogen  !-
         REAL specific_root_length  ! length of root per unit weight ! mm/g
         REAL lape                  ! Leaf area per plant at emergence  !m2 pl-1
         Real nplsb                 ! Number of plants in seedbed   !pl m-2
         Real nplds                 ! Number of plants direct-seeded in main field  !pl m-2
         REAL zrttr                 ! Root length/depth at transplanting  !m
         REAL TMPSB                 ! Temperature increase caused by greenhouse use (over seedbed)  !°C
         REAL TBD                   ! Base temperature for development   !°C
         REAL TOD                   ! Optimum temperature for development   !°C
         REAL TMD                   ! Maximum temperature for development   !°C
         REAL DVRJ                  ! Development rate during juvenile phase  !°Cd-1
         REAL DVRI                  ! Development rate during photoperiod-sensitive phase  !°Cd-1
         REAL DVRP                  ! Development rate during panicle development phase  !°Cd-1
         REAL DVRR                  ! Development rate in reproductive phase (post anthesis)  !°Cd-1
         REAL PPSE                  ! Photoperiod sensitivity   !h-1
         REAL MOPP                  ! Maximum optimum photoperiod   !h
         REAL SHCKD                 ! Delay parameter in phenology   !°Cd (°Cd)-1
         REAL FRPAR                 ! Fraction of short-wave radiation that is photosynthetically active  !-
         REAL CO2REF                ! Reference level of atmospheric CO2 (340 ppm)  !ppm
         REAL SCP                   ! Scattering coefficient of leaves for photosynthetically active radiation  !-
         REAL tclstr                ! Time coefficient for loss of stem reserves  !d-1
         REAL Q10                   ! Factor accounting for increase in maintenance respiration with a 10 °C rise in temperature  !-
         REAL TREF                  ! Reference temperature  !°C
         REAL MAINLV                ! Maintenance respiration coefficient of leaves  !kg CH2O kg-1 DM d-1
         REAL MAINSO                ! Maintenance respiration coefficient of storage organs  !kg CH2Okg-1 DM d-1
         REAL MAINST                ! Maintenance respiration coefficient of stems  !kg CH2O kg-1 DM d-1
         REAL MAINRT                ! Maintenance respiration coefficient of roots  !kg CH2O kg-1 DM d-1
         REAL CRGLV                 !Carbohydrate requirement for leaf dry matter production  !kg CH2O kg-1 DM
         REAL CRGST                 !Carbohydrate requirement for stem dry matter production  !kg CH2O kg-1 DM
         REAL CRGSO                 !Carbohydrate requirement for storage organ dry matter production  !kg CH2O kg-1 DM
         REAL CRGRT                 !Carbohydrate requirement for root dry matter production  !kg CH2O kg-1 DM
         REAL CRGSTR                !Carbohydrate requirement for stem reserves production  !kg CH2O kg-1 DM
         REAL FSTR                  !Fraction of carbohydrates allocated to stems, stored as reserves  !-
         REAL LRSTR                 !Fraction of allocated stem reserves that is available for growth  !-
         REAL NH                    !Number of hills   !hills m-2
         REAL NPLH                  !Number of plants per hill   !pl hill-1
         REAL SPGF                  !Spikelet growth factor   !no. kg-1
         REAL WGRMX                 !Maximum individual grain weight  !kg grain-1
         REAL ASLA                  !A parameter of function to calculate SLA  !-
         REAL BSLA                  !B parameter of function to calculate SLA  !-
         REAL CSLA                  !C parameter of function to calculate SLA  !-
         REAL DSLA                  !D parameter of function to calculate SLA  !-
         REAL SLAMAX                !Maximum value of specific leaf area  !ha leaf kg-1 leaf
         REAL RGRLMX                !Minimum value of relative growth rate of leaf area  !°Cd-1
         REAL RGRLMN                !Maximum value of relative growth rate of leaf area  !°Cd-1
         REAL SHCKL                 !Delay parameter in development   !°Cd (°Cd)-1
         REAL FCLV                  !Mass fraction of carbon in leaves  !kg C kg-1 DM
         REAL FCST                  !Mass fraction of carbon in stems  !kg C kg-1 DM
         REAL FCSO                  !Mass fraction of carbon in storage organs  !kg C kg-1 DM
         REAL FCRT                  !Mass fraction of carbon in roots  !kg C kg-1 DM
         REAL FCSTR                 !Mass fraction of carbon in stem reserves  !kg C kg-1 DM
         REAL GZRT                  !Growth rate of root length  !m d-1
         REAL ZRTMCW                !Maximum root length/depth as crop characteristic without drought  !m
         REAL ZRTMCD                !Maximum root length/depth as crop characteristic under drought  !m
         REAL ZRTMS                 !Maximum depth that roots can penetrate into soil  !m
         REAL ULLS                  !Upper limit of leaf rolling  !pF
         REAL LLLS                  !Lower limit of leaf rolling  !pF
         REAL ULDL                  !Upper limit of drought-induced dead leaves  !pF
         REAL LLDL                  !Lower limit of drought-induced dead leaves  !pF
         REAL ULLE                  !Upper limit of leaf expansion  !pF
         REAL LLLE                  !Lower limit of leaf expansion  !pF
         REAL ULRT                  !Upper limit of relative transpiration  !pF
         REAL LLRT                  !Lower limit of relative transpiration  !pF
         REAL NMAXUP                ! Maximum daily N uptake (kg N ha-1 d-1)
         REAL RFNLV                 !Residual N fraction of leaves  !kg N kg-1 DM
         REAL FNTRT                 !Fraction of N translocated from stems and leaves, which is translocated from roots to storage organs  !-
         REAL RFNST                 !Residual N fraction of stems  !kg N kg-1 DM
         REAL TCNTRF                !Time coefficient for N translocation to grains  !d
         REAL NFLVI                 !Initial nitrogen fraction in leaves on leaf area basis  !g N m-2 leaf
         REAL FNLVI                 !Initial fraction of N in leaves on weight basis  !kg N kg-1 DM
         REAL NMAXSO                !Maximum N fraction in storage organs  !kg N kg-1 DM
         REAL WCST(max_layer)       !Array of soil water content at saturation, per soil layer  !m3 m-3
         REAL WCFC(max_layer)       !Array of soil water content at field capacity, per soil layer  !m3 m-3
         REAL WCWP(max_layer)       !Array of soil water content at wilting point, per soil layer  !m3 m-3
         REAL WCAD(max_layer)       !Array of soil water content at air dryness, per soil layer  !m3 m-3
         real tkl(max_layer)        !Array of thickness of soil layers, per soil layer  !m
         real FVPD(max_table)       !Interpolated VPD stress factor (0-1)
         REAL eff(max_table)        !Initial light-use efficiency   !(kg CO2 ha-1 leaf h-1)(W m-2 leaf )-1
         real VPD(max_table)        !vapour pressure deficit for interpolation (hPa)
         REAL efft(max_table)       !Table of initial light-use efficiency as a function of temperature
         real ssga(max_table)       !Specific green stem area  !ha stem kg-1 stem
         REAL ssgat(max_table)      !Table of specific green stem area as a function of development stage
         real kdf(max_table)        !Extinction coefficient for leaves  !-
         REAL kdft(max_table)       !Table of extinction coefficient for leaves as a function of development stage  !-
         real redf(max_table)       !Index of effect of temperature on AMAX
         REAL redft(max_table)      !Table of effect of temperature on AMAX as a function of temperature
         real knf(max_table)        !Extinction coefficient of nitrogen profile in canopy  !-
         REAL knft(max_table)       !Table of extinction coefficient of nitrogen profile in canopy as a function of development stage
         real nflv(max_table)       !Nitrogen fraction in leaves on leaf area basis  !g N m-2 leaf
         REAL nflvt(max_table)      !Table of nitrogen fraction in leaves on leaf area basis as a functoion of development stage
         real fsh(max_table)        !Fraction of total dry matter allocated to shoots  !-
         REAL fsht(max_table)       !Table of fraction of total dry matter allocated to shoots as a function of development stage
         real flv(max_table)        !Fraction of shoot dry matter allocated to leaves  !-
         REAL flvt(max_table)       !Table of fraction of shoot dry matter allocated to leaves as a function of development stage
         real fst(max_table)        !Fraction of shoot dry matter allocated to stems  !-
         REAL fstt(max_table)       !Table of fraction of shoot dry matter allocated to stems as a function of development stage
         real fso(max_table)        !Fraction of shoot dry matter allocated to storage organs  !-
         REAL fsot(max_table)       !Table of fraction of shoot dry matter allocated to storage organs as a function of development stage
         real drlv(max_table)       !Leaf death coefficient
         REAL drlvt(max_table)      !Table of feaf death coefficient as a function of development stage
         real nsllv(max_table)      !N stress factor that accelerates leaf death  !-
         REAL nsllvt(max_table)     !Table of N stress factor that accelerates leaf death as a function of development stage
         real sla(max_table)        !Specific leaf area   !ha leaf kg-1 leaf
         REAL slat(max_table)       !Table of specific leaf area as a function of development stage
         real nminso(max_table)     !Minimum N fraction in storage organs  !kg N kg-1 DM
         REAL nminsot(max_table)    !Table of minimum N fraction in storage organs as a function of the amount of N in the crop till flowering (kg N ha-1)
         real nmaxl(max_table)      !Maximum N fraction in leaves on weight basis  !kg N kg-1 DM
         REAL nmaxlt(max_table)     !Table of maximum N fraction in leaves on weight basis as a function of development stage
         real nminl(max_table)      !Minimum N fraction in leaves on weight basis  !kg N kg-1 DM
         REAL nminlt(max_table)     !Table of minimum N fraction in leaves on weight basis as a function of development stage

         Integer numnminso          !Number of values in the table of minimum N fraction in storage organs
         Integer numnmaxl           !Number of values in the tablke of maximum N fraction in leaves on weight basis
         Integer numnminl           !Number of values in the table of minimum N fraction in leaves on weight basis
         Integer numVPD             !Number of values in the table of vapour pressure deficit for interpolation
         Integer sbdur              !Duration of seedbed  !d
         Integer numeff             !Number of values in the table of initial light-use efficiency
         Integer numssga            !Number of values in the table of specific green stem area
         Integer numkdf             !Number of values in the table of extinction coefficient for leaves
         Integer numredf            !Number of values in the table of index of effect of temperature on AMAX
         Integer numknf             !Number of values in the table of extinction coefficient of nitrogen profile in canopy
         Integer numnflv            !Number of values in the table of fraction of shoot dry matter allocated to leaves
         Integer IACC               !Switch used to determine accuracy of assimilation calculations
         Integer numfsh             !Number of values in the table of fraction of total dry matter allocated to shoots
         Integer numflv             !Number of values in the table of fraction of shoot dry matter allocated to leaves
         Integer numfst             !Number of values in the table of fraction of shoot dry matter allocated to stems
         Integer numfso             !Number of values in the table of fraction of shoot dry matter allocated to storage organs
         Integer numdrlv            !Number of values in the table of leaf death coefficient
         Integer numnsllv           !Number of values in the table of N stress factor that accelerates leaf death
         INteger numsla             !Number of values in the table of specific leaf area

!!XX NB. these are not read in - initialised to zero, so are probably wrong..
         REAL WSOI                 ! Initial dry weight of storage organs  !kg ha-1
         REAL WLVGI                ! Initial dry weight of leaves  !kg ha-1
         REAL WSTI                 ! Initial dry weight of stems  !kg ha-1
         REAL WRTI                 ! Initial dry weight of roots  !kg ha-1

         real cttmax               ! temperature factor - was hard coded (22.0)
      end type oryzaParameters
! ===================================================================

      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (oryzaGlobals),pointer :: g
      type (oryzaParameters),pointer :: p
      type (IDsType), pointer :: id

      contains

*     ===========================================================
      subroutine oryza_read_cultivar_params ()
*     ===========================================================
      implicit none

*+  Purpose
*       Get cultivar parameters for named cultivar, from crop parameter file.

*+  Changes
*       090994 jngh specified and programmed

*+  Calls
                                       ! lu_src_sum

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'oryza_read_cultivar_params')

*+  Local Variables
!      character  string*200            ! output string
      integer    numvals               ! number of values read

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call write_string ('   - Reading '
     :                   // trim(g%cultivar)
     :                   // ' Cultivar Parameters')

      call read_real_var (
     :           g%cultivar         ! Section header
     :          ,'dvri'             ! Keyword
     :          ,'()'               ! Units
     :          ,p%dvri             ! Array
     :          ,numvals            ! Number of values returned
     :          ,0.0                ! Lower Limit for bound check
     :          ,100.0)             ! Upper Limit for bound check

      call read_real_var (
     :           g%cultivar         ! Section header
     :          ,'dvrj'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%dvrj               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,50.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           g%cultivar         ! Section header
     :          ,'dvrp'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%dvrp               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,50.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           g%cultivar         ! Section header
     :          ,'dvrr'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%dvrr               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,50.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           g%cultivar         ! Section header
     :          ,'mopp'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%mopp               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,50.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           g%cultivar         ! Section header
     :          ,'ppse'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%ppse               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,50.0)                 ! Upper Limit for bound check


      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine oryza_start_crop (variant)
*     ===========================================================
      implicit none

*+  Purpose
*       Start crop using parameters specified in passed record

*+  Changes
*     010994 jngh specified and programmed
*     090695 psc  add row spacing read
*     220696 jngh changed extract to collect

*+  Local Variables
      integer I
      integer numvals
      REAL    num_layers
      character esection*80

      integer, intent(in) :: variant
      type(SowType) :: Sow

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'oryza_start_crop')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

     
      if (g%plant_status.ne.status_out) then
         call fatal_error(err_user, 
     :      'Already in the ground - did you forget to "end_crop"?')
      else
         call publish_null(id%sowing)

         call oryza_read_param ()
         
         call unpack_Sow(variant, Sow)
         g%cultivar = Sow%cultivar
         
         call oryza_read_cultivar_params ()

         p%ESTAB = Sow%Establishment
         
         g%plant_status = status_alive
         
         !Set CROPSTA: 0=before sowing; 1=sowing; 2=in seedbed;
         !             3=day of transplanting; 4=main growth period      
         g%CROPSTA = 1
         
         g%Rlai = 0.0

!     dsg 280408  Read in the appropriate establishment parameters, depending on the method specified
         IF (p%ESTAB.EQ.'transplant' ) then

!              sbdur - duration of the seed on nursery bed (days)
               p%sbdur = Sow%sbdur
               if (p%sbdur.eq.0) then 
                call fatal_error(err_user, 
     :          'In the sowing line, please specify a value for SBDUR')
               endif


!              nplh - number of plants per hill ()
               p%nplh = Sow%nplh

               if (p%nplh.eq.0) then 
                call fatal_error(err_user, 
     :          'In the sowing line, please specify a value for NPLH')
               endif

!              nh - number of hills ()
               p%nh = Sow%nh
      
               if (p%nh.eq.0) then 
                call fatal_error(err_user, 
     :          'In the sowing line, please specify a value for NH')
               endif

!              nplsb - number of plants in seed-bed ()
               p%nplsb = Sow%nplsb
      
               if (p%nplsb.eq.0) then 
                call fatal_error(err_user, 
     :          'In the sowing line, please specify a value for NPLSB')
               endif

               g%Rlai= p%lape * p%nplsb
            
            
         ELSEIF (p%ESTAB.EQ.'direct-seed') then

!              nplds - number of plants in seed-bed ()
               p%nplds = Sow%nplds


               if (p%nplds.eq.0) then 
                call fatal_error(err_user, 
     :          'In the sowing line, please specify a value for NPLDS')
               endif

               g%Rlai= p%lape * p%nplds
         else 
               call fatal_error(err_user, 
     :                 'unknown establishment ' // p%ESTAB)
         endif
  
 
         
         g%WLVG = 0.01  !initial Dry weight of green leaves  !kg ha-1
         g%WSTS = 0.01  !initial Dry weight of structural stems  !kg ha-1
         g%FNLV   = p%FNLVI
         g%FNST   = 0.5*p%FNLVI
         g%NFLV   = p%NFLVI
         g%ZRT    = 0.0001
      endif
      
      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine oryza_end_crop ()
*     ===========================================================
      implicit none

*+  Purpose
*       End crop

*+  Mission Statement
*     End the crop

*+  Changes
*       290994 jngh specified and programmed
*      191099 jngh changed to plant_Send_Crop_Chopped_Event

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'oryza_end_crop')

*+  Local Variables
      real       dm_residue            ! dry matter added to residue (g/m^2)
      real       N_residue             ! nitrogen added to residue (g/m^2)
      real       dm_root(1)               ! dry matter added to soil (g/m^2)
      real       N_root(1)                ! nitrogen added to soil (g/m^2)
      character  string*400            ! output string
      real       yield                 ! grain wt (kg/ha)
      real       fraction_to_Residue(3)   ! fraction sent to residue (0-1)
      real       dlt_dm_crop(3) ! change in dry matter of crop (kg/ha)
      real       dlt_dm_N(3)    ! N content of changeed dry matter (kg/ha)
      character  dm_type(3)*10
      data       dm_type/'stem','leaf','grain'/
      type (FOMLayerType) :: IncorpFOM
      integer layer

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (g%plant_status.ne.status_out) then
         call publish_null(id%harvesting)

         g%plant_status = status_out

         ! report
         write (string, '(3x, a, f7.1, a)')
     :          ' Crop ended. Rough Rice Yield (dw) = ',
     :          g%WRR, ' (kg/ha)'
         call Write_string (string)

         ! Incorporate roots to soiln module
         dm_root(1) = g%wrt  ! All roots in top layer. what a fudge.
         ! Approximate N content of roots from stem N ratio
         N_root(1)  = g%wrt * divide(g%anst, g%wst, 0.0)

         IncorpFOM%Type = p%crop_type
         IncorpFOM%num_layer = 1
         IncorpFOM%layer(1)%LabileP = 0.0
         IncorpFOM%layer(1)%CNR = 0.0
         IncorpFOM%layer(1)%FOM%Amount = dm_root(1)
         IncorpFOM%layer(1)%FOM%N = n_root(1)
         IncorpFOM%layer(1)%FOM%C = 0.0
         IncorpFOM%layer(1)%FOM%P = 0.0
         IncorpFOM%layer(1)%FOM%AshAlk = 0.0
		 
         call publish_FOMLayer(id%incorp_fom, IncorpFOM)

         ! put stover into surface residue
         dlt_dm_crop(1) = g%wst
         dlt_dm_crop(2) = g%wlv
         dlt_dm_crop(3) = 0.0  !(g%wrr if not harvested)

         dlt_dm_N(1) = g%anst
         dlt_dm_N(2) = g%anlv
         dlt_dm_N(3) = 0.0     !(g%anso " " ")

         fraction_to_residue(:) = 1.0

         call new_postbox ()
         call post_char_var   (DATA_crop_type
     :                           ,'()'
     :                           , p%crop_type)
         call post_char_array (DATA_dm_type
     :                           ,'()'
     :                           , dm_type, 3)
         call post_real_array (DATA_dlt_crop_dm
     :                           ,'(kg/ha)'
     :                           , dlt_dm_crop, 3)
         call post_real_array (DATA_dlt_dm_n
     :                           ,'(kg/ha)'
     :                           , dlt_dm_n, 3)
         call post_real_array (DATA_fraction_to_Residue
     :                           ,'()'
     :                           , fraction_to_Residue, 3)

         call event_send (unknown_module, EVENT_Crop_Chopped)
         call delete_postbox ()

         N_residue = sum(dlt_dm_N)
         dm_residue = sum(dlt_dm_crop)
         write (string, '(22x, a, f7.1, a, 3(a, 22x, a, f7.1, a))')
     :                    '  straw residue = '
     :                  , dm_residue , ' (kg/ha)'
     :                  , new_line
     :                  , '  straw N       = '
     :                  , N_residue , ' (kg/ha)'

     :                  , new_line
     :                  , '  root residue  = '
     :                  , dm_root(1) , ' (kg/ha)'
     :                  , new_line
     :                  , '  root N        = '
     :                  , N_root(1) , ' (kg/ha)'
         call write_string (string)

         call oryza_zero_variables ()
         call oryza_init ()

         g%plant_status = status_out

      else
          call warning_error (ERR_USER,
     :            'ORYZA is not in the ground -'
     :          //' unable to end crop.')
      endif

      call pop_routine (my_name)
      return
      end subroutine


* ====================================================================
       subroutine oryza_Create ()
* ====================================================================
      implicit none


*+  Purpose
*     Create oryza module. Doesn't do much apart from zero variables

*+  Mission Statement
*     Create oryza

*+  Changes
*     190599 jngh removed reference to version

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'oryza_create')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call oryza_zero_variables ()

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine oryza_Init ()
* ====================================================================
      implicit none
*+  Purpose
*     Initialise oryza module. The rest of the system is plugged in now.

*+  Mission Statement
*     Initialise oryza

*+  Changes
*     190599 jngh removed reference to version

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'oryza_init')

      character section_name*(*)
      parameter (section_name = 'parameters')

*+  Local variables
      Integer I,num_layers, numvals
      REAL zrti
      real dummy(max_layer)

*- Implementation Section ----------------------------------
      call push_routine (myname)

! set rlv at emergence         
         num_layers = count_of_real_vals (p%tkl, max_layer)
         do 10 I = 3,num_layers
         
            g%root_weight_layer(I) = 0.0
            g%root_length_layer(I) = 0.0
            g%rlv(I)= 0.0
            g%rlv(1) = 0.005
            g%rlv(2) = 0.001
 10     continue
      g%LRAV   = 1.
      g%LDAV   = 1.
      g%LEAV   = 1.
      g%LESTRS = 1.
      g%PCEW   = 1.
      g%CPEW   = 1.
      g%LRSTRS = 1.
      g%LDSTRS = 1.
      g%TRW    = 0.
      g%NSLLV  = 1.
      g%RNSTRS = 1.

      call get_real_var (unknown_module, 'latitude', '()'
     :                                  , g%lat, numvals
     :                                  , -90.0, 90.0)

      call read_char_var (
     :           section_name         ! Section header
     :          ,'crop_type'          ! Keyword
     :          ,'()'                 ! Units
     :          ,p%crop_type          ! Array
     :          ,numvals)             ! Number of values returned

      call read_char_var_optional (
     :           section_name         ! Section header
     :          ,'uptake_source'      ! Keyword
     :          ,'()'                 ! Units
     :          ,p%uptake_source      ! Array
     :          ,numvals)             ! Number of values returned

      if (numvals .eq. 0) then
         ! nothing in our parameters - see if swim3 is plugged in
         call get_real_var_optional(unknown_module
     :                              ,'swim3'
     :                              ,'()'
     :                              ,dummy(1)
     :                              ,numvals
     :                              ,0.0
     :                              ,1.0)
         if (numvals .gt. 0) then
           p%uptake_source = 'swim3'
         else
           p%uptake_source = 'calc'
         endif  
      endif

      if (p%uptake_source .eq. 'calc') then
         ! see whether soil is available to us.
         call get_real_array_optional (unknown_module, 'sw', max_layer
     :                                       , '(mm3/mm3)'
     :                                       , dummy, numvals
     :                                       , 0.0, 1.0)
         if (numvals.eq.0) then
           p%PRODENV=env_potential         ! No soilwat - non-limited
           call write_string('Non - limiting Soil Water conditions')
           ! set up (dummy) variables
           g%TKLT = 100.
           p%ZRTMS = 100.
           p%WCST(1)  = 0.3
           p%tkl(1)   = 100.0
         else
           p%PRODENV=env_limited
         endif
      elseif (p%uptake_source .eq. 'apsim' .or.
     :        p%uptake_source .eq. 'swim3') then
         p%PRODENV=env_limited 
      else
         call fatal_error(err_user, 
     :          'unknown uptake source ' // trim(p%uptake_source))
      endif

      ! see whether nitrogen is available to us.

      call get_real_array_optional (unknown_module, 'no3', max_layer
     :                                    , '(kg/ha)'
     :                                    , g%no3, numvals
     :                                    , 0.0, 10000.0)
      if (numvals.eq.0) then
        p%nitroenv=env_potential         ! No soiln - non-limited
        call write_string('Non - limiting Soil Nitrogen conditions')
      else
        p%nitroenv=env_limited
      endif

      call read_char_var_optional (section_name
     :                   ,'eo_source', '()'
     :                   , g%eo_source
     :                   , numvals)
      if (numvals .le. 0) then
          g%eo_source = 'eo'
      else
          call write_string('Eo taken from '//g%eo_source)
      endif

      call pop_routine (myname)
      return
      end subroutine

!
* ====================================================================
       subroutine oryza_zero_variables ()
* ====================================================================
      implicit none


*+  Purpose
*     Set all variables in this module to zero.

*+  Mission Statement
*     Zero variables

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'oryza_zero_variables')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)

! Zero everything _except_ what comes to us through _Init() and onNewProfile().

      g%anso=0.0
      g%anld=0.0
      g%ANCRF  = 0.
      g%anlv=0.
      g%anst=0.
      g%ANCR=0.
      g%NLV    = 0.
      g%NST    = 0.
      g%NSO    = 0.
      g%FNSO   = 0.
      g%NACR=0.0
      g%TRRM =0.0
      g%ZLL=0.0
      g%TNSOIL = 0.
      g%TRW=0.0
      g%cropsta = 0
      g%DAE = 0
      g%Idoy=0
      g%NGRM2  =0.0
      g%NSPM2  =0.0
      g%ZRTM=0.0
      g%PARCUM =0.0
      g%PARCM1 =0.0
      g%NGR = 0.
      g%RTNASS=0.
      g%TNASS=0.
      g%CKCIN=0.0
      g%CKCFL=0.0
      g%DLDRT=0.
      g%DLDR=0.
      g%TSLV=0.
      g%GLAI=0.
      g%RNSTRS=1.0
      g%LDSTRS=1.0
      g%tmmx = 0.0
      g%tmmn = 0.0
      g%tmda = 0.0
      g%ZRT = 0.0
      g%etd = 0.0
      g%RLAI = 0.0
      g%max_rlai = 0.0
      g%etrd = 0.0
      g%etae = 0.0
      g%ETD = 0.0
      g%trc  = 0.0
      g%WLVG = 0.0
      g%WSTS = 0.0
      g%WST  = 0.0
      g%WSTR = 0.0
      g%WLV  = 0.0
      g%WLVD = 0.0
      g%WAG  = 0.0
      g%WSO  = 0.0
      g%WAGT = 0.0
      g%TDRW = 0.0
      g%WRT  = 0.0
      g%DVS  = 0.0
      g%LESTRS = 1.0
      g%LRSTRS = 1.0
      g%TMPCOV = 0.0
      g%TAV    = 0.0
      g%TAVD   = 0.0
      g%DTR    = 0.0
      g%TMAX    = 0.0
      g%TMIN   = 0.0
      g%hu     = 0.0
      g%hulv   = 0.0
      g%NCOLD  = 0
      g%DVR    = 0.0
      g%dayl   = 24.0
      g%TS     = 0.0
      g%TSHCKD = 0.0
      g%LAIROL = 0.0
      g%ARLAI  = 0.0
      g%SAI    = 0.0
      g%KDF   = 0.0
      g%REDF = 0.0
      g%KNF = 0.0
      g%NFLV = 0.0
      g%CSLV  = 0.0
      g%ECPDF = 0.0
      g%GAI   = 0.0
      g%GPCDT = 0.0
      g%DTGA  = 0.0
      g%lat = 0.0
      g%COSLD = 0.0
      g%sinld = 0.0
      g%DAYLP = 0.0
      g%DSINB=0.0
      g%DSINBE=0.0
      g%SOLCON=0.0
      g%ANGOT=0.0
      g%RAPCDT = 0.
      g%HOUR=0.0
      g%SINB=0.0
      g%RAPSHL=0.0
      g%RAPPPL=0.0
      g%FSLLA=0.0
      g%AMAX2=0.0
      g%EFF=0.0
      g%EFF2=0.0
      g%GPSHL=0.0
      g%GPL=0.0
      g%RAPL=0.0
      g%PARI1 =0.0
      g%DPARI =0.0
      g%DPAR  =0.0
      g%PCEW=1.0
      g%CPEW=1.0
      g%FRT=0.0
      g%GGR=0.0
      g%PWRR=0.0
      g%WRR=0.0
      g%GCR=0.0
      g%NGCR  =0.0
      g%PLTR =0.0
      g%GRT    =0.
      g%GLV    =0.
      g%RWLVG  =0.
      g%GST    =0.
      g%GSTR   =0.
      g%RWSTR  =0.
      g%GSO    =0.
      g%GNSP =0.
      g%GNGR=0.
      g%NSP=0.0
      g%RGRL=0.0
      g%TRWL(:) = 0
      g%MSUC(:)  = 0.
      g%MSKPA(:) = 0.
!      p%wcst(:)=0.0
      g%WCL(:)=0.0
      g%FACT(:)=0.0
      g%ZRTL(:)  =0.0
      g%root_weight_layer(:) = 0.0
      g%rlv(:)= 0.0 
      g%root_length_layer(:) = 0.0
      g%ANSTA = 0.0
      g%ANLVA = 0.0
      g%plant_status = status_out
      g%NALVS = 0.0
      g%NASTS = 0.0
      g%NASOS = 0.0
      g%NACRS = 0.0
      g%NTRTS = 0.0
      g%COLDTT = 0.0
      g%GRAINS = .FALSE.
      g%SF1    = 1.
      g%SF2    = 1.
      g%SPFERT = 1.
      g%TFERT  = 0.
      g%NTFERT = 0.
      g%X       = 1.
      g%TESTL   = .FALSE.
      g%TESTSET = 0.00001
      g%WLVGEXS = 0.0
      g%LAIEXS  = 0.0
      g%TSLVTR  = 0.0
      g%TSHCKL  = 0.0
      g%WLVGEXP = 0.0
      g%LAIEXP  = 0.0
      g%no3(:) = 0.0
      g%nh4(:) = 0.0
      g%amax1 = 0.0   !! XXnot used anywhere??
      g%eff1=0.0      !! XXnot used anywhere??
      g%fnlv=0.0

      ! dsg 030809 rat grazing work with Peter Brown
      g%rat_graze_perc=0.0
      g%gr_rat_grazed = 0.0      
      g%LLV_RAT = 0.0

      p%lape = 0.0
      p%nplsb = 0.0
      p%nplds = 0.0
      p%zrttr = 0.0
      p%TMPSB = 0.0
      p%TBD = 0.0
      p%TOD = 0.0
      p%TMD = 0.0
      p%DVRJ = 0.0
      p%DVRI = 0.0
      p%DVRP = 0.0
      p%DVRR = 0.0
      p%PPSE = 0.0
      p%MOPP = 0.0
      p%SHCKD = 0.0
      p%FRPAR = 0.0
      p%CO2REF = 0.0
      p%SCP = 0.0
      p%tclstr = 0.0
      p%Q10 = 0.0
      p%TREF = 0.0
      p%MAINLV = 0.0
      p%MAINSO  =0.0
      p%MAINST  =0.0
      p%MAINRT  =0.0
      p%CRGLV   =0.0
      p%CRGST   =0.0
      p%CRGSO   =0.0
      p%CRGRT   =0.0
      p%CRGSTR  =0.0
      p%FSTR    =0.0
      p%LRSTR   =0.0
      p%NH      =0.0
      p%NPLH    =0.0
      p%SPGF    =0.0
      p%WGRMX   =0.0
      p%ASLA    =0.0
      p%BSLA    =0.0
      p%CSLA    =0.0
      p%DSLA    =0.0
      p%SLAMAX  =0.0
      p%RGRLMX  =0.0
      p%RGRLMN  =0.0
      p%SHCKL   =0.0
      p%FCLV    =0.0
      p%FCST    =0.0
      p%FCSO    =0.0
      p%FCRT    =0.0
      p%FCSTR   =0.0
      p%GZRT    =0.0
      p%ZRTMCW  =0.0
      p%ZRTMCD  =0.0
      p%ZRTMS   =0.0
      p%ULLS    =0.0
      p%LLLS    =0.0
      p%ULDL    =0.0
      p%LLDL    =0.0
      p%ULLE    =0.0
      p%LLLE    =0.0
      p%ULRT    =0.0
      p%LLRT    =0.0
      p%NMAXUP  =0.0
      p%RFNLV   =0.0
      p%FNTRT   =0.0
      p%RFNST   =0.0
      p%TCNTRF  =0.0
      p%NFLVI   =0.0
      p%FNLVI   =0.0
      p%NMAXSO  =0.0
      p%FVPD(:) = 0.0
      p%eff(:) = 0.0
      p%VPD(:) = 0.0
      p%efft(:) = 0.0
      p%ssga(:) = 0.0
      p%ssgat(:) = 0.0
      p%kdf(:) = 0.0
      p%kdft(:) = 0.0
      p%redf(:) = 0.0
      p%redft(:) = 0.0
      p%knf(:) = 0.0
      p%knft(:) = 0.0
      p%nflv(:) = 0.0
      p%nflvt(:) = 0.0
      p%fsh(:) = 0.0
      p%fsht(:) = 0.0
      p%flv(:) = 0.0
      p%flvt(:) = 0.0
      p%fst(:) = 0.0
      p%fstt(:) = 0.0
      p%fso(:) = 0.0
      p%fsot(:) = 0.0
      p%drlv(:) = 0.0
      p%drlvt(:) = 0.0
      p%nsllv(:) = 0.0
      p%nsllvt(:) = 0.0
      p%sla(:) = 0.0
      p%slat(:) = 0.0
      p%nminso(:) = 0.0
      p%nminsot(:) = 0.0
      p%nmaxl(:) = 0.0
      p%nmaxlt(:) = 0.0
      p%nminl(:) = 0.0
      p%nminlt(:) = 0.0

! Comes to us in onNewMet()
!      p%tkl(:) = 0.0
!      p%WCST(:) = 0.0
!      p%WCFC(:) = 0.0
!      p%WCWP(:) = 0.0
!      p%WCAD(:) = 0.0
!      g%TKLT=0.0

      p%numnminso =0
      p%numnmaxl  =0
      p%numnminl  =0
      p%numVPD    =0
      p%sbdur     =0
      p%numeff    =0
      p%numssga   =0
      p%numkdf    =0
      p%numredf   =0
      p%numknf    =0
      p%numnflv   =0
      p%IACC      =0
      p%numfsh    =0
      p%numflv    =0
      p%numfst    =0
      p%numfso    =0
      p%numdrlv   =0
      p%numnsllv  =0
      p%numsla    =0
      p%swirtr    =0
      p%prodenv   =0
      p%nitroenv  =0


      p%uptake_source = ' '
      p%crop_type = ' '
      p%ESTAB = ' '
      p%WSOI  =0.0
      p%WLVGI =0.0
      p%WSTI  =0.0
      p%WRTI  =0.0

      g%eo_source = ' '
      g%cultivar = ' '

      call pop_routine (myname)
      return
      end subroutine




* ====================================================================
       subroutine oryza_Send_my_variable (Variable_name)
* ====================================================================
      implicit none

*+  Sub-Program Arguments
       character Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*      Return the value of one of our variables to caller

*+  Mission Statement
*     Send Value of Requested Variable

*+  Changes
*     <insert here>

*+  Calls


*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'oryza_send_my_variable')

*+  Local Variables
       integer num_layers              ! no. of rlv layers
       real    cover_green
       real    cover_tot
       real    height
!       real ep
*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (variable_name .eq. 'plant_status') then
         call respond2get_char_var (variable_name
     :                             , '()'
     :                             , g%plant_status)

      elseif (variable_name .eq. 'crop_type') then

         call respond2get_char_var (
     :               variable_name     ! variable name
     :              ,'()'              ! variable units
     :              ,p%crop_type)      ! variable

      elseif (variable_name .eq. 'lrstrs') then
!              drought stress factor causing leaf rolling
         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'()'          ! variable units
     :              ,g%lrstrs)         ! variable

      elseif (variable_name .eq. 'ldstrs') then
!              drought stress factor accelerating leaf death
         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'()'          ! variable units
     :              ,g%ldstrs)         ! variable

      elseif (variable_name .eq. 'lestrs') then
!              drought stress reducing leaf expansion
         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'()'          ! variable units
     :              ,g%lestrs)         ! variable

      elseif (variable_name .eq. 'pcew') then
!             Effect of drought stress on daily total gross CO2 assimilation of crop;
!             reduction in potential transpiration rate
         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'()'          ! variable units
     :              ,g%pcew)         ! variable

      elseif (variable_name .eq. 'nflv') then
!             Nitrogen fraction in leaves on leaf area basis
         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'()'          ! variable units
     :              ,g%nflv)         ! variable

      elseif (variable_name .eq. 'fnlv') then
!             Fraction of N in leaves on weight basis
         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'()'          ! variable units
     :              ,g%fnlv)         ! variable

      elseif (variable_name .eq. 'nacr') then
!             Actual nitrogen uptake rate by crop (kg N ha-1 d-1)
         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'()'          ! variable units
     :              ,g%nacr)         ! variable

      elseif (variable_name .eq. 'tnsoil') then
!             Daily amount of N available for uptake from soil (kg N ha-1 d-1)
         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'()'          ! variable units
     :              ,g%tnsoil)         ! variable

      elseif (variable_name .eq. 'wso') then
!             Dry weight of storage organs (kg/ha)
         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'(kg/ha)'          ! variable units
     :              ,g%wso)         ! variable

      elseif (variable_name .eq. 'wst') then
!             Dry weight of stems  (kg/ha)
         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'()'          ! variable units
     :              ,g%wst)         ! variable

      elseif (variable_name .eq. 'wrt') then
!             Dry weight of roots  (kg/ha)
         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'(kg/ha)'          ! variable units
     :              ,g%wrt)         ! variable

      elseif (variable_name .eq. 'wrr') then
!             Dry weight of rough rice (final yield)  (kg/ha)
         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'(kg/ha)'          ! variable units
     :              ,g%wrr)         ! variable

      elseif (variable_name .eq. 'wlv') then
!             Dry weight of leaves  (kg/ha)
         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'(kg/ha)'          ! variable units
     :              ,g%wlv)         ! variable

      elseif (variable_name .eq. 'ancr') then
!             Amount of N in crop (live and dead material)(kg N/ha)
         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'()'          ! variable units
     :              ,g%ancr)         ! variable

      elseif (variable_name .eq. 'anso') then
!             Amount of N in storage organs

         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'()'          ! variable units
     :              ,g%anso)         ! variable

      elseif (variable_name .eq. 'wagt') then
!             Total aboveground dry matter (kg/ha)
         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'(kg/ha)'          ! variable units
     :              ,g%wagt)         ! variable

      elseif (variable_name .eq. 'rnstrs') then
!             Reduction factor on relative leaf growth rate caused by N stress
         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'()'          ! variable units
     :              ,g%rnstrs)         ! variable

      elseif (variable_name .eq. 'anlv') then
!              Amount of N in leaves (kg/ha)
         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'()'          ! variable units
     :              ,g%anlv)         ! variable

      elseif (variable_name .eq. 'wlvg') then
!             Dry weight of green leaves  (kg/ha)
         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'()'          ! variable units
     :              ,g%wlvg)         ! variable

      elseif (variable_name .eq. 'wlvd') then
!             Dry weight of dead leaves (kg/ha)
         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'()'          ! variable units
     :              ,g%wlvd)         ! variable

      elseif (variable_name .eq. 'dtga') then
!             Daily total gross CO2 assimilation of crop  (kg CO2 ha-1 d-1)
         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'()'          ! variable units
     :              ,g%dtga)         ! variable

      elseif (variable_name .eq. 'dvs') then
!             Development stage of crop
         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'()'          ! variable units
     :              ,g%dvs)         ! variable

      elseif (variable_name .eq. 'eff') then
!
         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'()'          ! variable units
     :              ,g%eff)         ! variable


      elseif (variable_name .eq. 'ssga') then

         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'()'          ! variable units
     :              ,g%ssga)         ! variable

      elseif (variable_name .eq. 'dvr') then

         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'()'          ! variable units
     :              ,g%dvr)         ! variable

      elseif (variable_name .eq. 'hu') then

         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'()'          ! variable units
     :              ,g%hu)         ! variable


      elseif (variable_name .eq. 'zrt') then
!           root length or rooting depth (m)
         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'(m)'          ! variable units
     :              ,g%zrt)         ! variable

      elseif (variable_name .eq. 'zll') then
!           summed root depths of preceding soil layers (m)
         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'(m)'          ! variable units
     :              ,g%zll)         ! variable

      elseif (variable_name .eq. 'etrd') then

         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'()'          ! variable units
     :              ,g%etrd)         ! variable

      elseif (variable_name .eq. 'etae') then

         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'()'          ! variable units
     :              ,g%etae)         ! variable

      elseif (variable_name .eq. 'trc') then

         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'()'          ! variable units
     :              ,g%trc)         ! variable

      elseif (variable_name .eq. 'trw') then
!            actual transpiration rate of crop with today's LAI
         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'()'          ! variable units
     :              ,g%trw)         ! variable

      elseif (variable_name .eq. 'gcr') then

         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'()'          ! variable units
     :              ,g%gcr)         ! variable

      elseif (variable_name .eq. 'rlai') then

         call respond2get_real_var (
     :               variable_name    ! variable name
     :              ,'()'          ! variable units
     :              ,g%rlai)         ! variable

      elseif (variable_name .eq. 'etd') then

         call respond2get_real_var (
     :               variable_name      ! variable name
     :              ,'(mm/day)'            ! variable units
     :              ,g%etd)            ! variable

       elseif (variable_name .eq. 'cropsta') then

         call respond2get_Integer_var (
     :               variable_name      ! variable name
     :              ,'()'            ! variable units
     :              ,g%cropsta)            ! variable


       elseif (variable_name .eq. 'dae') then

         call respond2get_integer_var (
     :               variable_name      ! variable name
     :              ,'()'            ! variable units
     :              ,g%dae)            ! variable

      elseif (variable_name .eq. 'cover_green') then

         cover_green = 1 - exp(-0.5 * g%rlai)
         call respond2get_real_var (
     :               variable_name      ! variable name
     :              ,'()'            ! variable units
     :              ,cover_green)            ! variable

      elseif (variable_name .eq. 'cover_tot') then

         cover_tot = 1 - exp(-0.5 * g%max_rlai)
         call respond2get_real_var (
     :               variable_name      ! variable name
     :              ,'()'            ! variable units
     :              ,cover_tot)            ! variable

      elseif (variable_name .eq. 'height') then

         height = 500.0
         call respond2get_real_var (
     :               variable_name      ! variable name
     :              ,'(mm)'            ! variable units
     :              ,height)            ! variable


      elseif (variable_name .eq. 'wcl') then
         num_layers = count_of_real_vals (p%tkl, max_layer)
         call respond2get_real_array (variable_name
     :                               , '(mm/mm3)'
     :                               , g%wcl
     :                               , num_layers)

      elseif (variable_name .eq. 'gnsp') then
         ! Rate of increase in spikelet number   !no. ha-1 d-1
         call respond2get_real_var (
     :               variable_name      ! variable name
     :              ,'(no. ha-1 d-1)'   ! variable units
     :              ,g%gnsp)            ! variable

      elseif (variable_name .eq. 'spgf') then
         ! Spikelet growth factor   !no. kg-1
         call respond2get_real_var (
     :               variable_name      ! variable name
     :              ,'(no. kg-1)'       ! variable units
     :              ,p%spgf)            ! variable

      elseif (variable_name .eq. 'coldtt') then
         ! Accumulated cold degree days (degree days)
         call respond2get_real_var (
     :               variable_name      ! variable name
     :              ,'(degree days)'       ! variable units
     :              ,g%coldtt)            ! variable

      elseif (variable_name .eq. 'sf1') then
         ! Spikelet sterility factor because of low temperatures   !-
         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'()'              ! variable units
     :              ,g%sf1)            ! variable

      elseif (variable_name .eq. 'sf2') then
         ! Spikelet fertility factor because of high temperatures   !-
         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'()'              ! variable units
     :              ,g%sf2)            ! variable

      elseif (variable_name .eq. 'spfert') then
         ! Spikelet fertility factor  !-
         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'()'              ! variable units
     :              ,g%spfert)            ! variable

      elseif (variable_name .eq. 'fso') then
         ! Fraction of shoot dry matter allocated to storage organs  !-
         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'()'              ! variable units
     :              ,g%fso)            ! variable

      elseif (variable_name .eq. 'gso') then
         ! Growth rate of storage organs  !kg DM ha-1 d-1 
         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'()'              ! variable units
     :              ,g%gso)            ! variable
      elseif (variable_name .eq. 'nsp') then
         ! Number of spikelets   !no. ha-1
         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(no. ha-1)'              ! variable units
     :              ,g%nsp)            ! variable

      elseif (variable_name .eq. 'gngr') then
         ! Rate of increase in grain number   !no. ha-1 d-1
         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(no. ha-1 d-1)'              ! variable units
     :              ,g%gngr)            ! variable

      elseif (variable_name .eq. 'ggr') then
         ! Rate of increase in grain weight  !kg DM ha-1 d-1
         call respond2get_real_var (
     :               variable_name      ! variable name
     :              ,'(kg DM ha-1 d-1)' ! variable units
     :              ,g%ggr)             ! variable

      elseif (variable_name .eq. 'ngr') then
         ! Number of grains  !no ha-1
         call respond2get_real_var (
     :               variable_name      ! variable name
     :              ,'(no ha-1)'        ! variable units
     :              ,g%ngr)             ! variable

      elseif (variable_name .eq. 'wgrmx') then
         ! Maximum individual grain weight  !kg grain-1
         call respond2get_real_var (
     :               variable_name      ! variable name
     :              ,'(kg grain-1)'        ! variable units
     :              ,p%wgrmx)             ! variable

! dsg 230507  apswim needs a 'sw_demand'
      elseif (variable_name .eq. 'sw_demand') then

         call respond2get_real_var (
     :               variable_name      ! variable name
     :              ,'(mm)'            ! variable units
     :              ,g%TRC)            ! variable

! dsg 230507  put in a dummy no3 demand to keep apwim happy
      elseif (variable_name .eq. 'no3_demand') then

         call respond2get_real_var (
     :               variable_name      ! variable name
     :              ,'(mm)'            ! variable units
     :              ,0.0)            ! variable

! dsg 230507  apswim needs an 'rlv'
      elseif (variable_name .eq. 'rlv') then
         num_layers = count_of_real_vals (p%tkl, max_layer)
         call respond2get_real_array (variable_name
     :                               , '(mm/mm3)'
     :                               , g%rlv
     :                               , num_layers)

      elseif (variable_name .eq. 'root_weight_layer') then
         num_layers = count_of_real_vals (p%tkl, max_layer)
         call respond2get_real_array (variable_name
     :                               , '(kg/ha)'
     :                               , g%root_weight_layer
     :                               , num_layers)

      elseif (variable_name .eq. 'root_length_layer') then
         num_layers = count_of_real_vals (p%tkl, max_layer)
         call respond2get_real_array (variable_name
     :                               , '()'
     :                               , g%root_length_layer
     :                               , num_layers)

      elseif (variable_name .eq. 'rat_grazing_perc') then

         call respond2get_real_var (
     :               variable_name      ! variable name
     :              ,'(%)'               ! variable units
     :              ,g%rat_graze_perc)  ! variable

      elseif (variable_name .eq. 'leaves_rat_grazed') then

         call respond2get_real_var (
     :               variable_name      ! variable name
     :              ,'()'               ! variable units
     :              ,g%LLV_RAT)  ! variable

      elseif (variable_name .eq. 'grains_rat_grazed') then

         call respond2get_real_var (
     :               variable_name      ! variable name
     :              ,'()'               ! variable units
     :              ,g%gr_rat_grazed)  ! variable
      else
         call Message_Unused ()
      endif

      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine oryza_read_param ()
*     ===========================================================
      implicit none

*+  Purpose
*       Read all module parameters.

*+  Mission Statement
*     Read parameters from parameter file

*+  Changes
*     <insert here>

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'oryza_read_param')
*
      character section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
      integer    numvals,maxirr,I               ! number of values read

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call write_string ('   - Reading Parameters')

      call read_char_var (
     :           section_name         ! Section header
     :          ,'crop_type'          ! Keyword
     :          ,'()'                 ! Units
     :          ,p%crop_type          ! Array
     :          ,numvals)             ! Number of values returned

!       dsg 290507  added (guess from sugar module
      call read_real_var (
     :           section_name                         ! Section header
     :          ,'specific_root_length'               ! Keyword
     :          ,'()'                                 ! Units
     :          ,p%specific_root_length               ! Array
     :          ,numvals                              ! Number of values returned
     :          ,0.0                                  ! Lower Limit for bound check
     :          ,200000.0)                               ! Upper Limit for bound check
      call read_real_var (
     :           section_name         ! Section header
     :          ,'nmaxup'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%nmaxup               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,10.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'rfnlv'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%rfnlv               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,10.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'fntrt'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%fntrt               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,10.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'rfnst'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%rfnst               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,10.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'tcntrf'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%tcntrf               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,50.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'nflvi'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%nflvi               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,5.0)                 ! Upper Limit for bound check


      call read_real_var (
     :           section_name         ! Section header
     :          ,'fnlvi'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%fnlvi               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,5.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'nmaxso'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%nmaxso               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,5.0)                 ! Upper Limit for bound check


      call read_real_var_optional (
     :           section_name         ! Section header
     :          ,'ulrt'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%ulrt               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,5000.0)                 ! Upper Limit for bound check
      if (numvals .gt. 0) then
        call read_real_var (
     :           section_name         ! Section header
     :          ,'llrt'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%llrt               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,5000.0)                 ! Upper Limit for bound check

        p%SWIRTR = ET_EXPONENTIAL ! Woperis
      else

        p%SWIRTR = ET_LINEAR      ! tanner & sinclair relationship, p91
      endif

      call read_real_var (
     :           section_name         ! Section header
     :          ,'ulle'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%ulle               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,5000.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'llle'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%llle               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,5000.0)                 ! Upper Limit for bound check


      call read_real_var (
     :           section_name         ! Section header
     :          ,'uldl'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%uldl               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,5000.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'lldl'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%lldl               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,5000.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'ulls'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%ulls               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,5000.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'llls'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%llls               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,5000.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'zrtms'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%zrtms               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'GZRT'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%GZRT               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,50.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'zrtmcw'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%zrtmcw               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,500.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'zrtmcd'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%zrtmcd               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,500.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'fcrt'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%fcrt               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,500.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'fclv'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%fclv               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,500.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'fcst'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%fcst               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,500.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'fcstr'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%fcstr               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,500.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'fcso'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%fcso               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,500.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'fstr'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%fstr               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'SHCKL'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%shckl               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'rgrlmx'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%rgrlmx               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1000.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'rgrlmn'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%rgrlmn               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,500.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'slamax'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%slamax               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1000.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'ASLA'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%asla               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,10.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'BSLA'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%bsla               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,10.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'CSLA'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%csla               ! Array
     :          ,numvals              ! Number of values returned
     :          ,-5.0                  ! Lower Limit for bound check
     :          ,10.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'DSLA'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%dsla               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,10.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'co2REF'               ! Keyword
     :          ,'(ppm)'              ! Units
     :          ,p%co2ref               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1000.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'SCP'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%scp               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1000.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'frpar'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%frpar               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1000.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'shckd'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%shckd               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,50.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'tclstr'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%tclstr               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,500.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'q10'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%q10               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,10.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'tref'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%tref               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'mainlv'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%mainlv               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'mainst'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%mainst               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'mainso'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%mainso               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'mainrt'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%mainrt               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'crglv'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%crglv               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1000.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'crgst'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%crgst               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1000.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'crgstr'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%crgstr               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1000.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'crgso'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%crgso               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1000.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'crgrt'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%crgrt               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'lrstr'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%lrstr               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,500.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'spgf'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%spgf               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100000.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'wgrmx'               ! Keyword
     :          ,'()'              ! Units
     :          ,p%wgrmx               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,500.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'zrttr'                 ! Keyword
     :          ,'()'              ! Units
     :          ,p%zrttr                 ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'tbd'                 ! Keyword
     :          ,'()'              ! Units
     :          ,p%tbd                 ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'tod'                 ! Keyword
     :          ,'()'              ! Units
     :          ,p%tod                 ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'tmd'                 ! Keyword
     :          ,'()'              ! Units
     :          ,p%tmd                 ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'tmpsb'                 ! Keyword
     :          ,'()'              ! Units
     :          ,p%tmpsb                 ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,10.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'lape'             ! Keyword
     :          ,'(mm)'               ! Units
     :          ,p%lape             ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,10.)              ! Upper Limit for bound check

      call read_integer_var (
     :           section_name         ! Section header
     :          ,'IACC'             ! Keyword
     :          ,'()'               ! Units
     :          ,p%IACC             ! Array
     :          ,numvals              ! Number of values returned
     :          ,0                  ! Lower Limit for bound check
     :          ,10)              ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'nminso'                ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%nminso                ! Array
     :          ,p%numnminso             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)               ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'nminsot'               ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%nminsot               ! Array
     :          ,p%numnminso             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,2000.0)                 ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'nmaxl'                ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%nmaxl                ! Array
     :          ,p%numnmaxl             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,10.0)               ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'nmaxlt'               ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%nmaxlt               ! Array
     :          ,p%numnmaxl             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)                 ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'nminl'                ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%nminl                ! Array
     :          ,p%numnminl             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,10.0)               ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'nminlt'               ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%nminlt               ! Array
     :          ,p%numnminl             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,10.0)                 ! Upper Limit for bound check

      call read_real_array_optional (
     :           section_name         ! Section header
     :          ,'slat'                ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%slat                ! Array
     :          ,p%numsla             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)               ! Upper Limit for bound check
      if (p%numsla .gt. 0) then
        call read_real_array (
     :           section_name         ! Section header
     :          ,'sla'               ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%sla               ! Array
     :          ,p%numsla             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)                 ! Upper Limit for bound check
      else
        p%sla(:) = 0.0
      endif

      call read_real_array (
     :           section_name         ! Section header
     :          ,'efft'                ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%efft                ! Array
     :          ,p%numeff             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)               ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'eff'               ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%eff               ! Array
     :          ,p%numeff             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)                 ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'ssgat'                ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%ssgat                ! Array
     :          ,p%numssga             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)               ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'ssga'               ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%ssga               ! Array
     :          ,p%numssga             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)                 ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'vpd'                ! Keyword
     :          ,max_table            ! array size
     :          ,'(0-1)'              ! Units
     :          ,p%vpd                ! Array
     :          ,p%numvpd             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)               ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'fvpd'               ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%fvpd               ! Array
     :          ,p%numvpd             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)                 ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'kdft'                ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%kdft                ! Array
     :          ,p%numkdf             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)               ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'kdf'               ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%kdf               ! Array
     :          ,p%numkdf             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)                 ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'redft'                ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%redft                ! Array
     :          ,p%numredf             ! Number of values returned
     :          ,-20.0                  ! Lower Limit for bound check
     :          ,100.0)               ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'redf'               ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%redf               ! Array
     :          ,p%numredf             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)                 ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'knf'                ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%knf                ! Array
     :          ,p%numknf             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)               ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'knft'               ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%knft               ! Array
     :          ,p%numknf             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)                 ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'nflv'                ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%nflv                ! Array
     :          ,p%numnflv             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)               ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'nflvt'               ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%nflvt               ! Array
     :          ,p%numnflv             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)                 ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'fsh'                ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%fsh                ! Array
     :          ,p%numfsh             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)               ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'fsht'               ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%fsht               ! Array
     :          ,p%numfsh             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)                 ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'flv'                ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%flv                ! Array
     :          ,p%numflv             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)               ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'flvt'               ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%flvt               ! Array
     :          ,p%numflv             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)                 ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'fst'                ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%fst                ! Array
     :          ,p%numfst             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)               ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'fstt'               ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%fstt               ! Array
     :          ,p%numfst             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)                 ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'fso'                ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%fso                ! Array
     :          ,p%numfso             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)               ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'fsot'               ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%fsot               ! Array
     :          ,p%numfso             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)                 ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'drlv'                ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%drlv                ! Array
     :          ,p%numdrlv             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)               ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'drlvt'               ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%drlvt               ! Array
     :          ,p%numdrlv             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)                 ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'nsllv'                ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%nsllv                ! Array
     :          ,p%numnsllv             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)               ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'nsllvt'               ! Keyword
     :          ,max_table            ! array size
     :          ,'()'              ! Units
     :          ,p%nsllvt               ! Array
     :          ,p%numnsllv             ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)                 ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'cttmax'             ! Keyword
     :          ,'()'                 ! Units
     :          ,p%cttmax             ! value
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)               ! Upper Limit for bound check


      call pop_routine  (myname)
      return
      end subroutine


* ====================================================================
       subroutine Oryza_get_other_variables()
* ====================================================================
      implicit none

*+  Purpose
*      Get the values of variables from other modules

*+  Changes


*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'oryza_get_other_variables')

*+  Local Variables
      integer deepest_layer
      integer numvals,I

*- Implementation Section ----------------------------------
      call push_routine(myname)

      if (p%PRODENV.eq. env_limited) then
          call get_real_array(unknown_module, 'sw', max_layer
     :                                    , '(mm3/mm3)'
     :                                    , g%wcl, numvals
     :                                    , 0.0, 1.0)
      else
          g%wcl(:) = 1.0
      endif

      if (p%nitroenv.eq. env_limited) then
          call get_real_array (unknown_module, 'no3', max_layer
     :                                    , '(kg/ha)'
     :                                    , g%no3, numvals
     :                                    , 0.0, 10000.0)


!        dsg 280408   add nh4 to the picture....
!                     assume plant can get N from both no3 & nh4, using no3 as first preference
          call get_real_array (unknown_module, 'nh4', max_layer
     :                                    , '(kg/ha)'
     :                                    , g%nh4, numvals
     :                                    , 0.0, 10000.0)

       deepest_layer = find_layer_no (1000*g%zrt
     :                                  ,p%tkl
     :                                  ,max_layer)
       g%tnsoil = sum_real_array(g%no3, deepest_layer) +
     :            sum_real_array(g%nh4, deepest_layer)
      else
          g%TNSOIL = 10000.0
      endif

      call get_real_var (unknown_module, g%eo_source, '(mm)'
     :                                , g%etd, numvals
     :                                , 0.0, 100.0)

      call get_real_var_optional (unknown_module
     :                           , 'co2', '()'
     :                           , g%co2, numvals
     :                           , 0.0, 1000.0)
      if (numvals .eq. 0) then
        g%co2= p%CO2REF
      else
      endif

      call pop_routine(myname)
      return
      end subroutine

!----------------------------------------------------------------------!
! SUBROUTINE ORYZAMODELS                                               !
!                                                                      !
! Date     : 9-Oct-2003, Adapted by Xike Zhang                         !
! Purpose  : This subroutine is the simulation models.                 !
! DOY     R4  Day number since 1 January (day of year) (d)          I  !
! IDOY    I4  Day number within year of simulation (d)              I  !
! YEAR    R4  Year of simulation (y)                                I  !
! IYEAR   I4  Year of simulation (y)                                I  !
! STTIME  R4  Start day of simulation (d)                           I  !
! TIME    R4  Time of simulation (d)                                I  !
! DELT    R4  Time interval of integration (d)                      I  !
! LAT     R4  Latitude of site (dec.degr.)                          I  !
! WSTAT   C*  Status code from weather system (-)                   I  !
! WTRTER  L4  Flag whether weather can be used by model (-)         O  !
! RDD     R4  Daily shortwave radiation (kJ.m-2.d-1)                I  !
! VP      R4  Early morning vapour pressure (kPa)                   I  !
! WN      R4  Average wind speed (m.s-1)                            I  !
! RAIN    R4  Daily amount of rainfall (mm.d-1)                     I  !
! Subroutines called: Oryza_ET, Oryza_WSTRESS, Oryza_WNOSTRESS, Oryza_IRRIG, Oryza_PADDY             !
!                     Oryza_NSOIL, Oryza_NNOSTRESS, Oryza_NCROP, ORYZA1                  !
!----------------------------------------------------------------------!
      SUBROUTINE ORYZAMODEL()

        IMPLICIT NONE
        Integer I,num_layers

!-----Set CROPSTA: 0=before sowing; 1=day of sowing; 2=in seedbed;
!                  3=day of transplanting; 4=main growth period

         If(g%cropsta .GE.1)   g%DAE = g%DAE+1

         IF (g%CROPSTA .EQ. 3) g%CROPSTA = 4
         IF (g%CROPSTA .EQ. 2) THEN
            IF (g%DAE .EQ. p%SBDUR) g%CROPSTA = 3
         END IF
         IF (g%CROPSTA .EQ. 1) THEN
            IF (p%ESTAB.EQ.'transplant') THEN
               g%CROPSTA = 2
             elseIF (p%ESTAB.EQ.'direct-seed') THEN
               g%CROPSTA = 4
            END IF
         END IF

!-----Calculate potential soil evaporation and transpiration
        CALL Oryza_ET() !same arguments

        IF (p%PRODENV.EQ.env_limited) THEN
            CALL Oryza_WSTRESS()
        ELSE If (p%PRODENV.EQ.env_potential) THEN
            CALL Oryza_WNOSTRESS()
        ELSE
            call fatal_error(err_user,'unknown water environment??')
        ENDIF

        CALL ORYZA1()

        IF (p%NITROENV.EQ.env_limited) THEN
           CALL Oryza_NCROP()
        ELSE IF (p%NITROENV.EQ.env_potential) THEN
           CALL Oryza_NNOSTRESS()
        ELSE
           call fatal_error(err_user,'unknown nitrogen environment??')
        ENDIF
!
      RETURN
      END subroutine

!----------------------------------------------------------------------!
!  SUBROUTINE Oryza_ET                                                       !
!  Used in ORYZA model version 4.0                                     !
!  Date  : December 2001                                              !
!  Author: B.A.M. Bouman                                               !
!                                                                      !
!  Purpose: Calculates potential evaporation of soil/water layer and   !
!           potential transpiration of a crop. Calculations done with: !
!           Penman, Priestley-Taylor or Makkink subroutines.           !
!           All calculations pertain to the main field, and not to     !
!           the seedbed.                                              !
!                                                                      !
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      !
! name   type meaning (unit)                                     class !
! ----   ---- ---------------                                    ----- !
! ITASK   I4  Task that subroutine should perform (-)               I  !
! ANGA    R4  Angstrom parameter A                                  I  !
! ANGB    R4  Angstrom parameter B                                  I  !
! RDD     R4  Daily shortwave radiation (J.m-2.d-1)                 I  !
! VP      R4  Early morning vapour pressure (kPa)                   I  !
! WN      R4  Average wind speed (m.s-1)                            I  !
! LAT     R4  Latitude of site (dec.degr.)                          I  !
! IDOY    I4  Day number within year of simulation (d)              I  !
! ETMOD   C*  Name of subroutine to calculate E and T (-)           I  !
! CROPSTA I4  Crop stage (-)                                        I  !
! NL      I4  Number of soil layers (-)                             I  !
! FAOF    R4  Correction factor for E and T (FAO factor) (-)        I  !
! WCST    R4  Array of water content saturation / layer (m3 m-3)    I  !
! LAI     R4  Leaf Area Index (-)                                   I  !
! EVSC    R4  Potential soil evaporation (mm d-1)                   O  !

! TRC     R4  Potential transpiration of crop at given LAI (mm d-1) O  !
! RFS  !Surface reflection coefficient  !-
! SUBROUTINES called: Oryza_SETPMD, Oryza_SETMKD, Oryza_SETPTD                           !
!                                                                      !
! Files included: -                                                    !
!----------------------------------------------------------------------!
      SUBROUTINE Oryza_ET()
      IMPLICIT NONE
!
!!        Estimate radiation-driven and wind- and humidity-driven part
!
           g%etrd = 0.75*g%etd
           g%etae = g%etd-g%etrd

!
!
!!---- Calculate potential transpiration of rice in main field
      IF (g%CROPSTA .GE. 4) THEN
         g%TRC = g%ETRD*(1.-EXP(-0.5*g%rlai))+g%ETAE*MIN(2.0,g%rlai)
!!     There is no transpiration from main field before transplanting
      ELSE
         g%TRC = 0.
      END IF
!
      RETURN
      END Subroutine


!----------------------------------------------------------------------*
!  SUBROUTINE Oryza_SUBDD                                              *
!  Purpose: This subroutine calculates the daily amount of heat units  *
!           for calculation of the phenological development rate and   *
!           early leaf area growth.                                    *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! TMAX    R4  Daily maximum temperature (oC)                        I  *
! TMIN    R4  Daily minimum temperature (oC)                        I  *
! TBD     R4  Base temperature for development (oC)                 I  *
! TOD     R4  Optimum temperature for development (oC)              I  *
! TMD     R4  Maximum temperature for development (oC)              I  *
! HU      R4  Heat units (oCd d-1)                                  O  *
! TD  !Hourly temperature  !°C                                                                     *
! TM  !Mean daily temperature  !°C
! TT  !Daily increment in heat units   !°Cd d-1
!
!  FILE usage : none                                                   *
!----------------------------------------------------------------------*
       SUBROUTINE Oryza_SUBDD()! different arguments TMAX,TMIN,TBD,TOD,TMD,HU
       IMPLICIT NONE

!-----Local parameters
      REAL    TD, TM, TT
      INTEGER I

      TM = (g%TMAX+g%TMIN)/2.
      TT = 0.
      DO I = 1,24
         TD = TM+0.5*ABS(g%TMAX-g%TMIN)*COS(0.2618*FLOAT(I-14))

         IF ((TD.GT.p%TBD).AND.(TD.LT.p%TMD)) THEN
           IF (TD.GT.p%TOD)then
              TD = p%TOD- (TD-p%TOD)*
     :                divide(p%TOD-p%TBD, p%TMD-p%TOD, 0.0)
           ENDIF
          TT = TT+(TD-p%TBD)/24.
         END IF

      END DO
      g%HU = TT
      g%hulv=g%hu

      RETURN
      END Subroutine

!----------------------------------------------------------------------*
!  SUBROUTINE Oryza_SUBCD                                                    *
!  Purpose: This subroutine calculates number of days below a certain  *
!           average temperature (TAV), which is used to terminate the  *
!           simulation after a maximum number of cold days the crop    *
!           can survive.                                               *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! CROPSTA I4  Crop stage (-)                                        I  *
! TAV     R4  Average daily temperature (oC)                        I  *
! TIME    R4  Time of simulation (d)                                T  *
! NCOLD   R4  Number of cold days (-)                               O  *
!                                                                      *
!  FILE usage : none                                                   *
!----------------------------------------------------------------------*
      SUBROUTINE Oryza_SUBCD () ! Same arguments: CROPSTA,TAV,TIME,NCOLD

      IMPLICIT NONE
!-----Formal parameters

      IF (g%CROPSTA .EQ. 3) g%NCOLD = 0
      IF (g%TAV.LT.12.) THEN
         g%NCOLD = g%NCOLD+1
      ELSE
         g%NCOLD = 0
      END IF
!
      RETURN
      END Subroutine


!----------------------------------------------------------------------*
!  SUBROUTINE Oryza_PHENOL                                                   *
!  Purpose: This subroutine calculates the rate of phenological        *
!           development of the crop based on photoperiod and           *
!           temperature.                                               *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! DVS     R4  Development stage of the crop (-)                     I  *
! DVRJ    R4  Development rate juvenile ((oCd)-1)                   I  *
! DVRI    R4  Development rate, photoperiod-sensitive phase         I  *
!             ((oCd)-1)                                                *
! DVRP    R4  Development rate, PI phase ((oCd)-1)                  I  *
! DVRR    R4  Development rate, reproductive phase ((oCd)-1)        I  *
! HU      R4  Heat units (oCd d-1)                                  I  *
! DAYL    R4  Astronomic daylength (base = 0 degrees) (h)           I  *
! MOPP    R4  Maximum optimum photoperiod (h)                       I  *
! PPSE    R4  Photoperiod sensitivity (h-1)                         I  *
! TS      R4  Temperature sum (oCd)                                 I  *
! SHCKD   R4  Delay parameter in phenology ((oCd)(oCd)-1)           I  *
! CROPSTA I4  Crop stage (-)                                        I  *
! DVR     R4  Development rate of the crop (d-1)                    O  *
! TSHCKD  R4  Transpl. shock for Oryza_PHENOL. development (oCd)          O  *
! DL  !Photoperiod daylength  !h                                                                     *
! PPFAC  !Factor determining photoperiod sensitivity  !-
! TSTR  !Temperature sum for phenological development at transplanting  !°Cd
!
!  FILE usage : none                                                   *
!----------------------------------------------------------------------*
      SUBROUTINE Oryza_PHENOL()!Same arguments: DVS,DVRJ,DVRI,DVRP,DVRR,HU,DAYL,MOPP,PPSE, &
                 !       TS,SHCKD,CROPSTA,DVR,TSHCKD


      IMPLICIT NONE
!-----Formal parameters

!-----Local parameters
      REAL    DL, TSTR, PPFAC

      IF (g%DVS.GE.0..AND.g%DVS.LT.0.40) g%DVR = p%DVRJ*g%HU
      IF (g%DVS.GE.0.40.AND.g%DVS.LT.0.65) THEN
         DL = g%DAYL+0.9
         IF (DL.LT.p%MOPP) THEN
            PPFAC = 1.
         ELSE
            PPFAC = 1.-(DL-p%MOPP)*p%PPSE
         END IF
         PPFAC = MIN(1.,MAX(0.,PPFAC))
         g%DVR   = p%DVRI*g%HU*PPFAC
      END IF
      IF (g%DVS.GE.0.65.AND.g%DVS.LT.1.00) g%DVR = p%DVRP*g%HU
      IF (g%DVS.GE.1.00)                   g%DVR = p%DVRR*g%HU

      IF (g%CROPSTA .EQ. 3)then
         TSTR = g%TS
         g%TSHCKD = p%SHCKD*TSTR
      ENDIF
      IF (g%CROPSTA .GT. 3 .AND.g%TS.LT.(TSTR+g%TSHCKD)) g%DVR = 0.

      RETURN
      END Subroutine

!----------------------------------------------------------------------*
! SUBROUTINE Oryza_SASTRO                                                    *
! Authors: Daniel van Kraalingen                                       *
! Date   : 12-June-1996, Version: 1.1                                  *
! Purpose: This subroutine calculates solar constant, daily            *
!          extraterrestrial radiation, daylength and some intermediate *
!          variables required by other routines. The routine has been  *
!          written such that latitudes from pole to pole can be used.  *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning                                     units  class *
! ----   ---- -------                                     -----  ----- *
! IDOY    I4  Day of year (Jan 1st = 1)                      d      I  *
! LAT     R4  Latitude of the site                       degrees    I  *
! SOLCON  R4  Solar constant at day=IDOY                   W/m2     O  *
! ANGOT   R4  Daily extraterrestrial radiation            J/m2/d    O  *
! DAYL    R4  Astronomical daylength (base = 0 degrees)      h      O  *
! DAYLP   R4  Photoperiodic daylength (base = -4 degrees)    h      O  *
! DSINB   R4  Daily total of sine of solar height            s      O  *
! DSINBE  R4  Daily integral of sine of solar height         s      O  *
!             corrected for lower transmission at low                  *
!             elevation                                                *
! SINLD   R4  Intermediate variable for subroutine Oryza_SSKYC     -      O  *
! COSLD   R4  Intermediate variable for subroutine Oryza_SSKYC     -      O  *
! ZZA  !Intermediate variable  !-                                                                     *
! Fatal error checks: LAT > 90, LAT < -90                              *
! Warnings          : LAT above polar circle, LAT within polar circle  *
! Subprograms called: FATALERR                                         *
! File usage        : none                                             *
!----------------------------------------------------------------------*

      SUBROUTINE Oryza_SASTRO() !Diffferent arguments
       !g%IDOY,p%lat, g%SOLCON,g%ANGOT,g%DAYL,g%DAYLP,g%DSINB,g%DSINBE,g%SINLD,g%COSLD

      IMPLICIT NONE
!     Local parameters
      REAL PI, DEGTRAD, DOY, DEC, ZZA, AOB,ZZCOS,ZZSIN

!     PI and conversion factor from degrees to radians
      PARAMETER (PI=3.1415927, DEGTRAD=0.017453292)

      DOY = g%IDOY
!
!!     Declination of the sun as a function of daynumber,
!!     calculation of daylength from intermediate variables
!!     SINLD, COSLD and AOB
!
      DEC=-ASIN(SIN(23.45*DEGTRAD)*COS(2.*PI*(DOY+10.)/365.))
      g%SINLD = SIN (DEGTRAD*g%lat)*SIN (DEC)
      g%COSLD = COS (DEGTRAD*g%lat)*COS (DEC)
      AOB   = g%SINLD/g%COSLD
!
      IF (AOB.LT.-1.) THEN
          call warning_error (ERR_USER,
     :            ' Oryza_SASTRO: ' //
     :            'latitude above polar circle, daylength=0 hours')
         g%DAYL  = 0.
         ZZCOS = 0.
         ZZSIN = 1.
      ELSE IF (AOB.GT.1.) THEN
          call warning_error (ERR_USER,
     :            ' Oryza_SASTRO: ' //
     :            'latitude within polar circle, daylength=24 hours')
         g%DAYL  = 24.
         ZZCOS =  0.
         ZZSIN = -1.
      ELSE
         g%DAYL  = 12.*(1.+2.*ASIN (AOB)/PI)
         g%DAYLP=12.0*(1.+2.*
     :         ASIN((-SIN(-4.*degtrad)+g%sinld)/g%cosld)/pi)
         ZZA   = PI*(12.+g%DAYL)/24.
         ZZCOS = COS (ZZA)
         ZZSIN = SIN (ZZA)
      END IF
!
!!     Daily integral of sine of solar height (DSINB) with a
!!     correction for lower atmospheric transmission at lower solar
!!     elevations (DSINBE)
!
      g%DSINB  = 2.*3600.*(g%DAYL*0.5*g%SINLD-12.*g%COSLD*ZZCOS/PI)
      g%DSINBE = 2.*3600.*(g%DAYL*(0.5*g%sinld+0.2*g%sinld**2+
     :           0.1*g%COSLD**2)-
     :          (12.*g%COSLD*ZZCOS+9.6*g%SINLD*g%COSLD*ZZCOS+
     :          2.4*g%COSLD**2*ZZCOS*ZZSIN)/PI)
!
!!     Solar constant and daily extraterrestrial radiation
      g%SOLCON = 1370.*(1.+0.033*COS (2.*PI*DOY/365.))
      g%ANGOT  = g%SOLCON*g%DSINB

      RETURN
      END Subroutine
!
!
!----------------------------------------------------------------------*
! SUBROUTINE Oryza_SSKYC                                                     *
! Authors: Daniel van Kraalingen                                       *
! Date   : 2-Jun-1993, Version 1.0                                     *
! Purpose: This subroutine estimates solar inclination and fluxes of   *
!          diffuse and direct irradiation at a particular time of      *
!          the day.                                                    *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning                                     units  class *
! ----   ---- -------                                     -----  ----- *
! HOUR    R4  Hour for which calculations should be done     h      I  *
! SOLCON  R4  Solar constant                               W/m2     I  *
! FRPAR   R4  Fraction of total shortwave irradiation        -      I  *
!             that is photosynthetically active (PAR)                  *
! DSINBE  R4  Daily integral of sine of solar height         s      I  *
!             corrected for lower transmission at low                  *
!             elevation                                                *
! SINLD   R4  Intermediate variable from Oryza_SASTRO              -      I  *
! COSLD   R4  Intermediate variable from Oryza_SASTRO              -      I  *
! RDD     R4  Daily shortwave radiation                   J/m2/d    I  *
! SINB    R4  Sine of solar inclination at HOUR              -      O  *
! RDPDR   R4  Instantaneous flux of direct photo-          W/m2     O  *
!             synthetically active radiation (PAR)                     *
! RDPDF   R4  Instantaneous flux of diffuse photo-         W/m2     O  *
!             synthetically active irradiation (PAR)                   *
! SOLHM  !Hour of day at which solar height is maximum  !h                                                                     *
! Fatal error checks: RDD <= 0                                         *
! Warnings          : ATMTR > 0.9                                      *
! Subprograms called: FATALERR                                         *
! File usage        : none                                             *
!----------------------------------------------------------------------*

      SUBROUTINE Oryza_SSKYC() !Same arguments: HOUR, SOLCON, FRPAR, DSINBE, SINLD, COSLD, RDD, &
                     !SINB, RDPDR, RDPDF. By Xike 14/10/03
      IMPLICIT NONE

!     Formal parameters

!     Local parameters
      REAL SOLHM,FRDIF,ATMTR,TMPR1
      character*80 string

!     Hour on day that solar height is at maximum
      PARAMETER (SOLHM=12.)

!!      IF (p%rdd.LE.0.) CALL FATALERR &
!         ('Oryza_SSKYC','total shortwave irradiation <= zero')
!
      ATMTR  = 0.
      FRDIF  = 0.
      g%RDPDF = 0.
      g%RDPDR = 0.
!
!!     Sine of solar inclination, 0.2617993 is 15 degrees in radians
      g%SINB = g%SINLD+g%COSLD*COS ((g%HOUR-SOLHM)*0.2617993)
!
      IF (g%SINB.GT.0.) THEN
!!        Sun is above the horizon
!
         TMPR1 = g%DTR*g%SINB*(1.+0.4*g%SINB)/g%DSINBE
         ATMTR = TMPR1/(g%SOLCON*g%SINB)
!
         IF (ATMTR.GT.0.92) then
            WRITE (string,'(A,G12.5,A)')
     :        ' Oryza_SSKYC: ATMTR =',
     :         ATMTR,', value very large'
            call warning_error (ERR_USER, string)
         ENDIF
         IF (ATMTR.LE.0.22) THEN
            FRDIF = 1.
         ELSE IF (ATMTR.GT.0.22 .AND. ATMTR.LE.0.35) THEN
            FRDIF = 1.-6.4*(ATMTR-0.22)**2
         ELSE
            FRDIF = 1.47-1.66*ATMTR
         END IF
!
!!        Apply lower limit to fraction diffuse
         FRDIF = MAX (FRDIF, 0.15+0.85*(1.-EXP (-0.1/g%SINB)))
!
!!        Diffuse and direct PAR
         g%RDPDF = TMPR1*p%FRPAR*FRDIF
         g%RDPDR = TMPR1*p%FRPAR*(1.-FRDIF)
!
      END IF

      RETURN
      END Subroutine

!----------------------------------------------------------------------*
! SUBROUTINE Oryza_SRDPRF                                                    *
! Authors: Daniel van Kraalingen                                       *
! Date   : 15-Sept-1994, Version: 1.0                                  *
! Purpose: This subroutine calculates the absorbed flux of radiation   *
!          for shaded leaves, the direct flux absorbed by leaves and   *
!          the fraction of sunlit leaf area.                           *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning                                     units  class *
! ----   ---- -------                                     -----  ----- *
! GAID    R4  Green area index above selected height    ha leaf/       *
!                                                     ha ground I      *
! CSLV    R4  Scattering coefficient of leaves for PAR       -      I  *
! SINB    R4  Sine of solar height                           -      I  *
! ECPDF   R4  Extinction coefficient for          ha ground/ha leaf I  *
!             diffuse light                                            *
! RDPDR   R4  Instantaneous flux of direct photo-          W/m2     I  *
!             synthetically active radiation (PAR)                     *
! RDPDF   R4  Instantaneous flux of diffuse photo-         W/m2     I  *
!             synthetically active irradiation (PAR)                   *
! RAPSHL  R4  Absorbed flux for shaded leaves           W/m2 leaf   O  *
! RAPPPL  R4  Direct flux absorbed by leaves            W/m2 leaf   O  *
!             perpendicular on direct beam                             *
! FSLLA   R4  Fraction of leaf area that is sunlit           -      O  *
! CLUSTF  !Cluster factor  !-                                                                     *
! ECPBL  !Extinction coefficient for direct radiation  !-
! ECPTD  !Extinction coefficient for direct component of direct radiation  !-
! RAPDDL  !Absorbed flux of direct component of direct radiation  !W m-2 leaf
! RAPDFL  !Absorbed flux of diffuse radiation  !W m-2 leaf
! RAPTDL  !Absorbed flux of total direct radiation  !W m-2 leaf
! RFLH  !Reflection coefficient of crop with horizontal leaf angle distribution  !-
! RFLS  !Reflection coefficient of crop with spherical leaf angle distribution  !-
!
! Fatal error checks: none                                             *
! Warnings          : none                                             *
! Subprograms called: none                                             *
! File usage        : none                                             *
!----------------------------------------------------------------------*

      SUBROUTINE Oryza_SRDPRF ()!Same arguments !GAID,CSLV,SINB,ECPDF,RDPDR,RDPDF, &
                   ! RAPSHL,RAPPPL,FSLLA
      IMPLICIT NONE

!     Formal parameters
!      REAL GAID  , CSLV  , SINB , ECPDF, RDPDR, RDPDF
!      REAL RAPSHL, RAPPPL, FSLLA

!     Local parameters
      REAL RFLH,RFLS,ECPBL,ECPTD,RAPDFL,RAPTDL,RAPDDL,CLUSTF,TMPR1

!!     Reflection of horizontal and spherical leaf angle distribution
      TMPR1 = SQRT (1. - g%CSLV)
      RFLH  = (1. - TMPR1) / (1. + TMPR1)
      RFLS  = RFLH * 2. / (1. + 2. * g%SINB)
!
!!     Extinction coefficient for direct radiation and total direct flux
      CLUSTF = g%ECPDF / (0.8*TMPR1)
      ECPBL  = (0.5/g%SINB) * CLUSTF
      ECPTD  = ECPBL * TMPR1
!
!!     Absorbed fluxes per unit leaf area: diffuse flux, total direct
!!     flux, direct component of direct flux
      RAPDFL = (1.-RFLH) * g%RDPDF * g%ECPDF * EXP(-g%ECPDF*g%GAID)
      RAPTDL = (1.-RFLS) * g%RDPDR * ECPTD * EXP (-ECPTD * g%GAID)
      RAPDDL = (1.-g%CSLV) * g%RDPDR * ECPBL * EXP (-ECPBL * g%GAID)
!
!!     Absorbed flux (J/m2 leaf/s) for shaded leaves
      g%RAPSHL = RAPDFL + RAPTDL - RAPDDL
!
!!     Direct flux absorbed by leaves perpendicular on direct beam
      g%RAPPPL = (1.-g%CSLV) * g%RDPDR / g%SINB
!
!!     Fraction sunlit leaf area (FSLLA)
      g%FSLLA = CLUSTF * EXP (-ECPBL*g%GAID)

      RETURN
      END Subroutine


! user subroutine !!!!!!!!????
      subroutine Oryza_GPParGet(xGAI
     :                         , xGAID
     :                         , xAmaxIn
     :                         , xEffIn
     :                         , xAmaxOut
     :                         , xEffOut
     :                         , cCO2
     :                         , cKNF
     :                         , cNFLV
     :                         , cREDFT)
      implicit none

!     formal parameters
      real xGAI      ! Total leaf area index
      real xGAID     ! Leaf area index above point of calculation
      real xAmaxIn   ! Uncorrected amax
      real xEffIn    ! Uncorrected efficiency
      real xAmaxOut  ! Corrected amax
      real xEffOut   ! Corrected efficiency
      real cCO2      ! Ambient CO2 level
      real cKNF
      real cNFLV
      real cREDFT

!     local variables
      real AmaxCO2,SLNI
      real Amax

!
       AmaxCO2 = 49.57/34.26*(1.-exp (-0.208*(cCO2-60.)/49.57))
       AmaxCO2 = max (0.,AmaxCO2)
!

      if(xGAI.GT.0.01 .AND. g%KNF.GT.0.) then
         SLNI = cNFLV*xGAI*cKNF*exp (-cKNF*xGAID)/
     :             (1.-exp (-cKNF*xGAI))
       else
         SLNI = cNFLV
      end if
!
!!-----Calculate actual photosynthesis from SLN, CO2 and temperature
!!     calculation of AMAX according to Van Keulen & Seligman (1987):
!!     AMAX = 32.4 * (SLNI-0.2) * REDFT * CO2AMX
!
      if (SLNI.GE.0.5) then
         !! According to Shaobing Peng (IRRI, unpublished data):
         Amax = 9.5+(22.*SLNI)*cREDFT*AmaxCO2
       else
         Amax = max (0.,68.33*(SLNI-0.2)*cREDFT*AmaxCO2)
      end if

      xAmaxOut = Amax
      xEffOut  = xEffIn
!
      return
      end Subroutine
!
!----------------------------------------------------------------------*
! SUBROUTINE Oryza_SGPL                                                      *
! Authors: Daniel van Kraalingen                                       *
! Date   : 15-Sept-1994, Version: 1.0                                  *
! Purpose: This subroutine calculates assimilation at a single depth   *
!          in the canopy                                               *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning                                     units  class *
! ----   ---- -------                                     -----  ----- *
! CSLV    R4  Scattering coefficient of leaves for visible   -      I  *
!             radiation (PAR)                                          *
! AMAX1   R4  Assimilation rate at light saturation       kg CO2/   I  *
!                                                        ha leaf/h     *
! ECPDF   R4  Extinction coefficient for diffuse light              I  *
! GAI     R4  Total green area                             ha/ha    I  *
! GAID    R4  Green area index above selected height       ha/ha    I  *
! SINB    R4  Sine of solar height                           -      I  *
! RDPDR   R4  Instantaneous flux of direct photo-          W/m2     O  *
!             synthetically active radiation (PAR)                     *
! RDPDF   R4  Instantaneous flux of diffuse photo-         W/m2     O  *
!             synthetically active irradiation (PAR)                   *
! GPL     R4  Instantaneous assimilation rate of          kg CO2/   O  *
!             leaves at depth GAI                        ha leaf/h     *
! RAPL    R4  Absorbed radiation at depth GAI            W/m2 leaf  O  *
! REAL    AMAX2  !Corrected CO2 assimilation rate at light saturation  !kg CO2 ha-1 leaf h-1                                                                      *
! Fatal error checks: none                                             *
! Warnings          : none                                             *
! Subprograms called: Oryza_SRDPRF                                           *
! File usage        : none                                             *
!----------------------------------------------------------------------*

      SUBROUTINE Oryza_SGPL()
      IMPLICIT NONE

!     Gauss parameters
      INTEGER IGSN
      PARAMETER (IGSN=3)
      REAL GSX(IGSN), GSW(IGSN)
      REAL TMPR1
!     Miscellaneous
      INTEGER I1

!     Gauss weights for three point Gauss
      DATA GSX /0.112702, 0.500000, 0.887298/
      DATA GSW /0.277778, 0.444444, 0.277778/

      g%EFF1 =g%EFF

      !! Selection of depth of canopy, canopy assimilation is set to zero
      CALL Oryza_SRDPRF() !Same arguments

      !! Get photosynthesis parameters from user routine
      CALL Oryza_GPPARGET(g%gai
     :                    ,g%GAID
     :                    ,g%AMAX1
     :                    ,g%eff1
     :                    ,g%AMAX2
     :                    ,g%eff2
     :                    ,g%CO2
     :                    ,g%KNF
     :                    ,g%NFLV
     :                    ,g%REDF)

      !! Assimilation of shaded leaf area
      IF (g%AMAX2.GT.0.) THEN
         g%GPSHL = g%AMAX2 * (1.-EXP (-g%RAPSHL*
     :                 divide(g%EFF2,g%AMAX2, 0.0)))
      ELSE
         g%GPSHL = 0.
      END IF
      !
      !!     Assimilation of sunlit leaf area
      !
      g%GPSLL  = 0.
      g%RAPSLL = 0.
      DO I1=1,IGSN
         TMPR1 = g%RAPSHL + g%RAPPPL * GSX(I1)
         IF (g%AMAX2.GT.0.) THEN
            g%GPSLL=g%GPSLL+g%AMAX2
     :              * (1.-EXP(-TMPR1*g%EFF2/g%AMAX2))
     :              * GSW(I1)
         ELSE
            g%GPSLL = 0.
         END IF
         g%RAPSLL = g%RAPSLL + TMPR1 * GSW(I1)
      END DO
!
!!     Local assimilation rate (GPL) and rate of
!!     absorption of PAR by canopy (RAPL)
      g%GPL  = g%FSLLA * g%GPSLL  + (1.-g%FSLLA) * g%GPSHL
      g%RAPL = g%FSLLA * g%RAPSLL + (1.-g%FSLLA) * g%RAPSHL

      RETURN
      END Subroutine
!
!----------------------------------------------------------------------*
! SUBROUTINE Oryza_SGPC1                                                     *
! Authors: Daniel van Kraalingen                                       *
! Date   : 15-Dec-1993, Version: 1.0                                   *
! Purpose: This subroutine performs a Gaussian integration over        *
!          depth of canopy by selecting three different GAI's and      *
!          computing assimilation at these GAI levels. The             *
!          integrated variable is GPC. Also absorbed PAR is            *
!          integrated in RAPC                                          *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning                                     units  class *
! ----   ---- -------                                     -----  ----- *
! CSLV    R4  Scattering coefficient of leaves for           -      I  *
!             visible radiation (PAR)                                  *
! AMAX    R4  Assimilation rate at light saturation       kg CO2/   I  *
!                                                        ha leaf/h     *
! EFF     R4  Initial light use efficiency               kg CO2/J/  I  *
!                                                        ha/h m2 s     *
! ECPDF   R4  Extinction coefficient for          ha ground/ha leaf I  *
!             diffuse light                                            *
! GAI     R4  Green area index                             ha/ha    I  *
! SINB    R4  Sine of solar height                           -      I  *
! RDPDR   R4  Instantaneous flux of direct photo-          W/m2     I  *
!             synthetically active radiation (PAR)                     *
! RDPDF   R4  Instantaneous flux of diffuse photo-         W/m2     I  *
!             synthetically active irradiation (PAR)                   *
! GPC     R4  Instantaneous assimilation rate of          kg CO2/   O  *
!             whole canopy                               ha soil/h     *
! RAPC    R4  Absorbed PAR                                 W/m2     O  *
!                                                                      *
! Fatal error checks: none                                             *
! Warnings          : none                                             *
! Subprograms called: Oryza_SGPL                                             *
! File usage        : none                                             *
!----------------------------------------------------------------------*

      SUBROUTINE Oryza_SGPC1()
      IMPLICIT NONE

!     Gauss parameters
      INTEGER IGSN
      PARAMETER (IGSN=3)
      REAL GSX(IGSN), GSW(IGSN)

!     Miscellaneous
      INTEGER I1

!     Gauss weights for three point Gauss
      DATA GSX /0.112702, 0.500000, 0.887298/
      DATA GSW /0.277778, 0.444444, 0.277778/

!!     Selection of depth of canopy, canopy assimilation is set to zero
      g%GPC  = 0.
      g%RAPC = 0.
!
      DO I1=1,IGSN
         g%GAID = g%GAI * GSX(I1)
!
         CALL Oryza_SGPL()
!
!!        Integration of local assimilation rate to canopy
!!        assimilation (GPC) and absorption of PAR by canopy (RAPC)
         g%GPC  = g%GPC  + g%GPL  * GSW(I1)
         g%RAPC = g%RAPC + g%RAPL * GSW(I1)
!
      END DO
!
      g%GPC  = g%GPC  * g%GAI
      g%RAPC = g%RAPC * g%GAI

      RETURN
      END Subroutine


!----------------------------------------------------------------------*
! SUBROUTINE Oryza_SGPC2                                                     *
! Authors: Daniel van Kraalingen                                       *
! Date   : 15-Dec-1993, Version: 1.1                                   *
! Purpose: Iterative procedure to calculate whole canopy assimilation  *
!          using internal error control. Routine can be used in place  *
!          of Oryza_SGPC1 for greater accuracy. The method used is a combi-  *
!          nation of the TRAPZD and QSIMP routines from Numerical      *
!          Recipes (Press Oryza_ET al., 1990, page 111-113)                  *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning                                     units  class *
! ----   ---- -------                                     -----  ----- *
! CSLV    R4  Scattering coefficient of leaves for          -       I  *
!             visible radiation (PAR)                                  *
! AMAX    R4  Assimilation rate at light saturation      kg CO2/    I  *
!                                                       ha leaf/h      *
! EFF     R4  Initial light use efficiency            kg CO2/J/ha/  I  *
!                                                       (h/m2/s)       *
! ECPDF   R4  Extinction coefficient for          ha ground/ha leaf I  *
!             diffuse light                                            *
! GAI     R4  Green area index                            ha/ha     I  *
! SINB    R4  Sine of solar height                          -       I  *
! RDPDR   R4  Instantaneous flux of direct photo-          W/m2     I  *
!             synthetically active radiation (PAR)                     *
! RDPDF   R4  Instantaneous flux of diffuse photo-         W/m2     I  *
!             synthetically active irradiation (PAR)                   *
! GPC     R4  Instantaneous assimilation rate of         kg CO2/    O  *
!             whole canopy                              ha soil/h      *
! RAPC    R4  Absorbed radiation                          W/m2      O  *
! DEL  !Intermediate variable for numerical integration  !-                                                                     *
! EPS  !Intermediate variable for numerical integration  !-
! GPCO  !Intermediate variable for numerical integration  !-
! GPCT  !Intermediate variable for numerical integration  !-
! GPCTO  !Intermediate variable for numerical integration  !-
! GPL  !Instantaneous CO2 assimilation rate of leaves at depth GAI  !kg CO2 ha-1 leaf h-1
! GPL1  !Intermediate variable for numerical integration  !-
! GPL2  !Intermediate variable for numerical integration  !-
! NMAX  !Intermediate variable for numerical integration  !-
! RAPCO  !Intermediate variable for numerical integration  !-
! RAPCT  !Intermediate variable for numerical integration  !-
! RAPCTO  !Intermediate variable for numerical integration  !-
! RAPL1  !Intermediate variable for numerical integration  !-
! RAPL2  !Intermediate variable for numerical integration  !-
! SUM1  !Intermediate variable for numerical integration  !-
! SUM2  !Intermediate variable for numerical integration  !-
! TNM  !Intermediate variable for numerical integration  !-
! Fatal error checks: N > NMAX (6)                                     *
! Warnings          : none                                             *
! Subprograms called: Oryza_SGPL, FATALERR                                   *
! File usage        : none                                             *
!----------------------------------------------------------------------*

      SUBROUTINE Oryza_SGPC2()
      IMPLICIT NONE
!
!!     Local variables
!
      REAL EPS, GPCTO, GPCT, RAPCT, RAPCTO, SUM1, SUM2, GPCO, RAPCO
      REAL DEL, TNM, X, GPL1, GPL2, RAPL1, RAPL2
      INTEGER NMAX, N, J, IT
!
      PARAMETER (EPS=0.10, NMAX=7)
!
      call fatal_error(err_user, 'Oryza_SGPC2 nyi')
!
!      g%GPC   = 0.
!      GPCO  = 1.
!      GPCT  = 0.
!!
!      g%RAPC  = 0.
!      RAPCO = 1.
!      RAPCT = 0.
!!
!      N = 1
!!
!      DO WHILE ((ABS (g%GPC-GPCO).GT.EPS*ABS (GPCO).OR.
!     :           ABS (g%RAPC-RAPCO).GT.EPS*ABS (RAPCO)))
!!
!         GPCO   = g%GPC
!         GPCTO  = GPCT
!!
!         RAPCO  = g%RAPC
!         RAPCTO = RAPCT
!!
!         IF (N.EQ.1) THEN
!            CALL Oryza_SGPL (g%CSLV, g%AMAX, g%EFF,g%ECPDF, g%GAI, 0., g%SINB, &
!                       g%RDPDR, g%RDPDF, GPL1 , RAPL1)
!            CALL Oryza_SGPL (g%CSLV, g%AMAX, g%EFF,g%ECPDF, g%GAI, g%GAI, g%SINB, &
!                       g%RDPDR, g%RDPDF, GPL2 , RAPL2)
!
!!           First approximation of total gross photosynthesis of canopy
!            GPCT  = 0.5*g%GAI*(GPL1 +GPL2)
!
!!           First approximation of absorption of PAR by canopy
!            RAPCT = 0.5*g%GAI*(RAPL1+RAPL2)
!
!         ELSE
!            IT   = 2**(N-2)
!            TNM  = IT
!            DEL  = g%GAI/TNM
!            X    = 0.5*DEL
!            SUM1 = 0.
!            SUM2 = 0.
!            DO J=1,IT
!              CALL Oryza_SGPL (g%CSLV, g%AMAX, g%EFF,g%ECPDF, g%GAI, X, g%SINB, &
!                         g%RDPDR, g%RDPDF, g%GPL2 , g%RAPL2)
!              SUM1 = SUM1+GPL2
!              SUM2 = SUM2+RAPL2
!              X    = X+DEL
!            END DO
!
!!           Higher order approximation of total gross photosynthesis
!!           of canopy
!            GPCT  = 0.5*(GPCT +g%GAI*SUM1/TNM)
!
!!           Higher order approximation of absorption of PAR by
!!           canopy
!            RAPCT = 0.5*(RAPCT+g%GAI*SUM2/TNM)
!
!         END IF
!
!         GPC  = (4.*GPCT -GPCTO)/3.
!         RAPC = (4.*RAPCT-RAPCTO)/3.
!
!         N = N+1
!!         IF (N.GT.NMAX) CALL FATALERR ('Oryza_SGPC2','too many steps')
!      END DO

      RETURN
      END subroutine

!----------------------------------------------------------------------*
! SUBROUTINE Oryza_SGPCDT                                                    *
! Authors: Daniel van Kraalingen                                       *
! Date   : 28-Apr-1995, Version: 1.2                                   *
! Purpose: This subroutine calculates daily total gross                *
!          assimilation (GPCDT) by performing a Gaussian integration   *
!          over time. At three different times of the day,             *
!          radiation is computed and used to determine assimilation    *
!          whereafter integration takes place. Accuracy of computation *
!          can be controlled the switch IACC (IACC=1 calls three point *
!          GAUSS procedure, IACC=2 calls combination routine with      *
!          error control                                               *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning                                     units  class *
! ----   ---- -------                                     -----  ----- *
! IACC    I4  Switch used to determine accuracy of            -     I  *
!             assimilation calculations                                *
! IDOY    I4  Day of year                                    d      I  *
! LAT     R4  Latitude of the site                        degrees   I  *
! RDD     R4  Daily total of shortwave radiation          J/m2/d    I  *
! FRPAR   R4  Fraction of total shortwave irradiation        -      I  *
!             that is photosynthetically active                        *
! CSLV    R4  Scattering coefficient of leaves for PAR        -     I  *
! AMAX    R4  Assimilation rate at light saturation       kg CO2/   I  *
!                                                        ha leaf/h     *
! EFF     R4  Initial light use efficiency               kg CO2/J/  I  *
!                                                       (ha/h/m2/s)    *
! ECPDF   R4  Extinction coefficient for          ha ground/ha leaf I  *
!             diffuse light                                            *
! GAI     R4  Green area index                    ha leaf/ha ground I  *
! DAYL    R4  Astronomic daylength (base = 0 degrees)        h      O  *
! DAYLP   R4  Photoperiodic daylength (base = -4 degrees)    h      O  *
! GPCDT   R4  Daily total gross assimilation            kg CO2/ha/d O  *
! RAPCDT  R4  Daily rate of absorbed PAR                  J/m/d     O  *
!                                                                      *
! Fatal error checks: none                                             *
! Warnings          : none                                             *
! Subprograms called: Oryza_SASTRO, Oryza_SGPC1, Oryza_SGPC2, Oryza_SSKYC                      *
! File usage        : none                                             *
!----------------------------------------------------------------------*

      SUBROUTINE Oryza_SGPCDT()
      IMPLICIT NONE

!     Gauss parameters
      INTEGER IGSN
      PARAMETER (IGSN=3)
      REAL GSX(IGSN), GSW(IGSN)

!     Miscellaneous
      INTEGER I1

      DATA GSX /0.112702, 0.500000, 0.887298/
      DATA GSW /0.277778, 0.444444, 0.277778/

      g%CSLV=p%SCP    !XXX ???
      g%ECPDF=g%KDF
      g%GAI=g%ARLAI

      !!     Compute daylength and related data
      CALL Oryza_SASTRO ()!Different arguments

      !!     Assimilation set to zero and three different times of the day
      g%GPCDT  = 0.
      g%RAPCDT = 0.
      DO I1=1,IGSN
         !!        At the specified HOUR, external radiation
         !!        conditions are computed
         g%HOUR = 12.0+g%DAYL*0.5*GSX(I1)
         CALL Oryza_SSKYC () !Same arguments

         !! Assimilation rate of canopy is computed
         IF (p%IACC.EQ.1) THEN
            !! Choose three point Gauss method
            CALL Oryza_SGPC1 ()
         ELSE IF (p%IACC.EQ.2) THEN
            !! Choose routine with internal error control
            CALL Oryza_SGPC2 ()
         ELSE
            CALL FATAL_ERROR (err_user,
     :             'Oryza_SGPCDT accuracy IACC not specified')
         END IF

         g%GPCDT  = g%GPCDT +g%GPC *GSW(I1)
         g%RAPCDT = g%RAPCDT+g%RAPC*GSW(I1)
      END DO

      !! Integration of assimilation rate to a daily total (GPCDT)
      g%GPCDT  = g%GPCDT *g%DAYL

      !! Integration of absorption rate to a daily total (RAPCDT)
      g%RAPCDT = g%RAPCDT*g%DAYL*3600.
      g%DTGA=g%GPCDT
      RETURN
      END Subroutine


!----------------------------------------------------------------------*
!  SUBROUTINE Oryza_SUBGRN                                                   *
!  Adapted: Bouman, July 1999                                          *
!  Purpose: This subroutine calculates spikelet formation rate and     *
!           spikelet fertility as affected by low and high temperature *
!           and the grain growth rate.  Spikelet sterility component   *
!           is according to Horie Oryza_ET al., 1992.                        *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! GCR     R4  Gross growth rate of the crop (kg DM/ha/d)            I  *
! CROPSTA R4  Crop stage (-)                                        I  *
! LRSTRS  R4  Leaf rolling stress factor (-)                        I  *
! DVS     R4  Development stage of the crop (-)                     I  *
! SF1     R4  Spikelet fertility factor due to low temperatures (-) I  *
! SF2     R4  Spikelet fertility factor due to high temperatures (-)I  *
! SPGF    R4  Spikelet growth factor (no kg-1)                      I  *
! TAV     R4  Average daily temperature (oC)                        I  *
! TMAX    R4  Daily maximum temperature (oC)                        I  *
! NSP     R4  Number of spikelets (no)                              I  *
! TIME    R4  Time of simulation (d)                                T  *
! GNSP    R4  Rate of increase in spikelet number (no ha-1 d-1)     O  *
! GNGR    R4  Rate of increase in grain number (no ha-1 d-1)        O  *
! SPFERT  R4  Spikelet fertility (-)                                O  *
! GRAINS  L*  Fortran logical function whether grains are formed    O  *
! COLDTT  !Accumulated cold degree days  !°Cd                                                                     *
! CTT  !Cold degree day  !°Cd
! DVSF  !Development stage of crop at flowering  !-
! DVSPI  !Development stage of crop at panicle initiation  !-
! NTFERT  !Number of days of flowering period  !d
! TFERT   !Average daily maximum temperature during flowering  !°C
! TINCR   !Temperature increase because of leaf rolling  !°C
!  FILE usage : none                                                   *
!----------------------------------------------------------------------*
      SUBROUTINE Oryza_SUBGRN()
      IMPLICIT NONE

!-----Local parameters
      REAL    DVSPI, DVSF, CTT,  TINCR

      !!-----Temperature increase due to leaf rolling (BAS): 1.6 degree
      !!     per unit leaf rolling (Turner Oryza_ET al., 1986; p 269)
      TINCR = 5.*(1.-g%LRSTRS)*1.6

      !!-----Spikelet formation between PI and Flowering
      DVSPI = 0.65
      DVSF  = 1.
      IF ((g%DVS.GE.DVSPI).AND.(g%DVS.LE.DVSF)) THEN
         g%GNSP = g%GCR*p%SPGF
      ELSE
         g%GNSP = 0.
      END IF

      !!-----Grain formation from spikelets (GNGR)
      !!-----Calculate GNGR reduction factors
      IF ((g%DVS.GE.0.75).AND.(g%DVS.LE.1.2)) THEN
         CTT    = MAX(0.,p%cttmax-(g%TAV-TINCR))
         g%COLDTT = g%COLDTT+CTT
      END IF

      IF ((g%DVS.GE.0.96).AND.(g%DVS.LE.1.2)) THEN
         g%TFERT  = g%TFERT +(g%TMAX+TINCR)
         g%NTFERT = g%NTFERT+1.
      END IF

      !!-----Apply GNGR reduction factors when DVS is 1.2
      IF((g%DVS.GE.1.2).AND.(.NOT.g%GRAINS)) THEN
         g%GRAINS = .TRUE.
         g%SF1    = 1.-(4.6+0.054*g%COLDTT**1.56)/100.
         g%SF1    = MIN(1.,MAX(0.,g%SF1))
         g%TFERT  = divide(g%TFERT,g%NTFERT, 0.0)
         g%SF2    = divide(1.,(1.+EXP(0.853*(g%TFERT-36.6))),0.0)
         g%SF2    = MIN(1.,MAX(0.,g%SF2))
         g%SPFERT = MIN(g%SF1,g%SF2)
         g%GNGR   = g%NSP*g%SPFERT
      ELSE
         g%GNGR   = 0.
      END IF

      RETURN
      END Subroutine


!----------------------------------------------------------------------*
!  SUBROUTINE Oryza_SUBLAI2                                                  *
!  Version 2: January 2001                                             *
!                                                                      *
!  Purpose: This subroutine calculates the rate of growth of LAI of    *
!    of the crop in the seedbed and after transplanting in the field.  *
!    Reductions by N-stress and water-stress are taken into account.   *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! CROPSTA I4  Crop stage (-)                                        I  *
! RWLVG   R4  Green leaves growth rate (kg d-1 ha-1)                I  *
! DLDR    R4  Death rate green leaves (kg d-1 ha-1)                 I  *
! TSLV    R4  Temperature sum for leaf development (oC)             I  *
! HULV    R4  Daily temperature for leaf development (oC)           I  *
! SHCKL   R4  Delay parameter in development ((oCd)(oCd)-1)         I  *
! LESTRS  R4  Reduction factor for leaf elongation (-)              I  *
! SLA     R4  Specific leaf area (ha kg-1)                          I  *
! NH      R4  Number of hills (hills m-2)                           I  *
! NPLH    I4  Number of plants per hill (pl/hill)                   I  *
! NPLSB   R4  Number of plants in seedbed (pl/m2)                   I  *
! DVS     R4  Development stage of the crop (-)                     I  *
! LAI     R4  Leaf area index (ha ha-1)                             I  *
! RGRLMX  R4  Maximum relative growth rate leaves ((oCd)-1)         I  *
! RGRLMN  R4  Minimum relative growth rate leaves ((oCd)-1)         I  *
! ESTAB   C*  Establishment method (-)                              I  *
! GLAI    R4  Growth rate leaf area index (ha d-1 ha-1)             O  *
! RGRL    R4  Actual relative growth rate leaves ((oCd)-1)          O  *
! GLAI1  !Intermediate value of GLAI  !ha ha-1 d-1
! GLAI2   !Intermediate value of GLAI  !ha ha-1 d-1
! LAIEXP  !Value of LAI at end of exponential growth phase after transplanting  !ha leaf ha-1 soil
! LAIEXS  !Value of LAI at end of exponential growth phase in seedbed  !ha leaf ha-1 soil
! TEST  !Difference between simulated and user-supplied SLA  !ha leaf kg-1 leaf
! TESTL  !Logical variable to indicate whether the difference between simulated and imposed SLA is smaller than TESTSET  !-
! TESTSET  !Maximum difference between simulated and user-supplied SLA  !ha leaf kg-1 leaf
! TSLVTR  !Temperature sum for leaf area development at transplanting  !°Cd
! WLVGEXP  !Value of WLVG at end of exponential growth phase after transplanting  !kg ha-1
! WLVGEXS  !Value of WLVG at end of exponential growth phase in seedbed  !kg ha-1
!
!
!----------------------------------------------------------------------*
!
      Subroutine Oryza_Sublai2()
!
      IMPLICIT NONE
!
!-----Local parameters
      real   GLAI1,GLAI2
      REAL NOTNUL

!!      Calculate RGRL as function of N stress limitation
       g%RGRL = p%RGRLMX - (1.-g%RNSTRS)*(p%RGRLMX-p%RGRLMN)

       IF (p%ESTAB .EQ. 'transplant') THEN
         !!===================================================================*
         !!------Transplanted rice                                            *
         !!===================================================================*
         !!------- 1. Seed-bed; no drought stress effects in seed-bed!
         IF (g%CROPSTA .LT. 3) THEN
            IF (g%RLAI.LT.1.) THEN
               g%GLAI    = g%RLAI*g%RGRL*g%HULV
               g%WLVGEXS = g%WLVG
               g%LAIEXS  = g%RLAI
            ELSE
               g%TEST = divide(ABS((g%RLAI/g%WLVG)-g%SLA),g%SLA,1.0)
               IF (.NOT. g%TESTL) THEN
                  IF (g%TEST .LT. g%TESTSET) g%TESTL = .TRUE.
               END IF
               IF (g%TESTL) THEN
                  g%GLAI = ((g%WLVG+g%RWLVG)*g%SLA)-g%RLAI
               ELSE
                  GLAI1 = ((g%WLVG+g%RWLVG-g%WLVGEXS)*
     :                            g%SLA+g%LAIEXS)-g%RLAI
                  GLAI2 = ((g%WLVG+g%RWLVG)*g%SLA)-g%RLAI
                  g%GLAI  = (GLAI1+g%X*GLAI2)/(g%X+1.)
                  g%X     = g%X+1.
               END IF
            END IF
         !!------- 2. Transplanting effects: dilution and shock-setting
         ELSE IF (g%CROPSTA .EQ. 3) THEN
            g%TSLVTR = g%TSLV
            g%TSHCKL = p%SHCKL*g%TSLVTR
            g%GLAI   = (g%RLAI * p%NH *
     :                    divide(p%NPLH, p%NPLSB, 0.0)) - g%RLAI
            g%TESTL  = .FALSE.
            g%X      = 1.
         !!--------3. After transplanting: main crop growth
         ELSE IF (g%CROPSTA .EQ. 4) THEN
            !!--------3.1. During transplanting shock-period
            IF (g%TSLV.LT.(g%TSLVTR+g%TSHCKL)) THEN
               g%GLAI = 0.
            !!--------3.2. After transplanting shock; drought stress effects
            ELSE
               IF ((g%RLAI.LT.1.0).AND.(g%DVS.LT.1.0)) THEN
                  g%GLAI = g%LESTRS * g%RLAI*g%RGRL*g%HULV
                  g%WLVGEXP = g%WLVG
                  g%LAIEXP  = g%RLAI
               ELSE
                  !! There is a transition from RGRL to SLA determined growth
                  !! when difference between simulated and imposed SLA is less than 10%
                  g%TEST =divide(ABS((g%RLAI/g%WLVG)-g%SLA),g%SLA,1.0)
                  IF (.NOT. g%TESTL) THEN
                     IF (g%TEST .LT. g%TESTSET) g%TESTL = .TRUE.
                  END IF
                  IF (g%TESTL) THEN
                     g%GLAI = ((g%WLVG+g%RWLVG-g%DLDR)*g%SLA)-g%RLAI
                  ELSE
                     GLAI1 = ((g%WLVG+g%RWLVG-g%DLDR-g%WLVGEXP)*g%SLA+
     :                         g%LAIEXP)-g%RLAI
                     GLAI2 = ((g%WLVG+g%RWLVG-g%DLDR)*g%SLA)-g%RLAI
                     g%GLAI  = (GLAI1+g%X*GLAI2)/(g%X+1.)
                     g%X     = g%X+1.
                  END IF
               END IF
            END IF
         END IF

       !!===================================================================*
       !!------Direct-seeded rice                                           *
       !!===================================================================*
       ELSE IF (p%ESTAB .EQ. 'direct-seed') THEN
         IF ((g%RLAI.LT.1.0).AND.(g%DVS.LT.1.0)) THEN
            g%GLAI    = g%RLAI*g%RGRL*g%HULV * g%LESTRS
            g%WLVGEXP = g%WLVG
            g%LAIEXP  = g%RLAI
         ELSE
            !! There is a transition from RGRL to SLA determined growth
            !! when difference between simulated and imposed SLA is less than 10%
            g%TEST = ABS((g%RLAI/g%WLVG)-g%SLA)/g%SLA
            IF (.NOT. g%TESTL) THEN
               IF (g%TEST .LT. g%TESTSET) g%TESTL = .TRUE.
            END IF
            IF (g%TESTL) THEN
               g%GLAI = ((g%WLVG+g%RWLVG-g%DLDR)*g%SLA)-g%RLAI
            ELSE
               GLAI1 = ((g%WLVG+g%RWLVG-g%DLDR-g%WLVGEXP)*g%SLA+
     :                   g%LAIEXP)-g%RLAI
               GLAI2 = ((g%WLVG+g%RWLVG-g%DLDR)*g%SLA)-g%RLAI
               g%GLAI  = (GLAI1+g%X*GLAI2)/(g%X+1.)
               g%X     = g%X+1.
            END IF
         END IF
       END IF

      RETURN
      END Subroutine


  !----------------------------------------------------------------------*
!  SUBROUTINE Oryza_SUBCBC                                                   *
!  Purpose: This subroutine checks the Crop Carbon Balance             *
!           and stops the simulation if the difference between         *
!           CKCIN and CKCFL exceeds 0.1 %                              *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! CKCIN   R4  Accumulated C in the crop (kg C ha-1)                 I  *
! CKCFL   R4  Sum of integrated C fluxes (kg C ha-1)                I  *
! TIME    R4  Time of simulation (d)                                T  *
! CBCHK   R4  Carbon balance check, relative value to the sums of      *
!             CKIN and CKCFL (-)                                    O  *
! TERMNL  R4  Flag to indicate if simulation is to stop (-)         O  *
!                                                                      *
!  FILE usage : none                                                   *
!----------------------------------------------------------------------*
      SUBROUTINE Oryza_SUBCBC()
      IMPLICIT NONE
!-----Formal parameters
      REAL CBCHK
      character string*200

      IF ((p%ESTAB.EQ.'transplant'.AND.g%CROPSTA.GE.3) .OR.
     :        (p%ESTAB.EQ.'direct-seed'.AND.g%CROPSTA.GE.1)) THEN

        CBCHK = 2.0*divide(g%CKCIN-g%CKCFL,
     :                     g%CKCIN+g%CKCFL+1.E-10,
     :                     0.0)
      Endif

      IF (ABS(CBCHK).GT.0.001) THEN
         WRITE (string,'(A,3(A,A,F8.4))')
     :           '* * * Error in Carbon Balance, please check * * *',
     :      new_line, ' CBCHK=',CBCHK,
     :      new_line, ' CKCIN=',g%CKCIN,
     :      new_line, ' CKCFL=',g%CKCFL

!         call fatal_error(err_user, string)
      END IF

      RETURN
      END Subroutine

!----------------------------------------------------------------------*
! SUBROUTINE ORYZA1                                                    *
! Rice crop growth module of ORYZA2000 model                           *
!                                                                      *
! Date      : November 2002                                            *
! History   : Adapted from ORYZA1 (1995), and ORYZA_W (1996) models    *
!             This version for release under FSEWin                    *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! ITASK   I4  Task that subroutine should perform (-)               I  *
! IUNITD  I4  Unit that can be used for input files (-)             I  *
! IUNITL  I4  Unit number for log file messages (-)                 I  *
! FILEI1  C*  Name of file with input model data (-)                I  *
! FILEIT  C*  Name of experimental data file (-)                    I  *
! OUTPUT  L4  Flag to indicate if output should be done (-)         I  *
! TERMNL  L4  Flag to indicate if simulation is to stop (-)        I/O *
! IDOY    I4  Day number within year of simulation (d)              I  *
! DOY     R4  Day number (January 1 = 1) (d)                        I  *
! TIME    R4  Time of simulation (d)                                T  *
! DELT    R4  Time step of integration (d)                          I  *
! LAT     R4  Latitude of site (dec.degr.)                          I  *
! RDD     R4  Daily shortwave radiation (J m-2 d-1)                 I  *
! TMMN    R4  Daily minimum temperature (degrees C)                 I  *
! TMMX    R4  Daily maximum temperature (degrees C)                 I  *
! NFLV    R4  N fraction in the leaves (g N m-2)                    I  *
! NSLLV   R4  N stress factor that accelerates leaf death (-)       I  *
! RNSTRS  R4  N stress reduction factor for RGRL (-)                I  *
! ESTAB   C*  Mode of establishment (-)                             I  *
! TKLT    R4  Thickness of combined soil layers (m)                 I  *
! ZRTMS   R4  Maximum rooting depth of soil (m)                     I  *
! CROPSTA I4  Crop stage (-)                                        I  *
! LRSTRS  R4  Leaf rolling stress factor (-)                        I  *
! LDSTRS  R4  Leaf death stress factor (-)                          I  *
! LESTRS  R4  Leaf expansion stress factor (-)                      I  *
! PCEW    R4  Reduction in potential transpiration rate (-)         I  *
! DAE     R4  Days after emergence (d)                              O  *
! LAIROL  R4  Leaf area index rolled (ha ha-1)                      O  *
! ZRT     R4  Rooting depth (m)                                     O  *
! DVS     R4  Development stage of the crop (-)                     O  *
! LLV     R4  Loss rate of leaves (kg ha-1 d-1)                     O  *
! DLDR    R4  Death rate of leaves caused by drought (kg ha-1 d-1)  O  *
! WLVG    R4  Dry weight of green leaves (kg ha-1)                  O  *
! WST     R4  Dry weight of stems (kg ha-1)                         O  *
! WSO     R4  Dry weight of storage organs (kg ha-1)                O  *
! GSO     R4  Growth rate of storage organs (kg ha-1 d-1)           O  *
! GGR     R4  Rate of increase in grain weight (kg ha-1 d-1)        O  *
! GST     R4  Growth rate of stems (kg ha-1 d-1)                    O  *
! GLV     R4  Growth rate of leaves (kg ha-1 d-1)                   O  *
! PLTR    R4  Intermediate variable for planting density (-)        O  *
! CO2EFF  !Relative effect of CO2 on initial light-use efficiency  !-                                                                     *
! CO2LV  !CO2 production factor for growth of leaves  !kg CO2 kg-1 DM
! CO2RT  !CO2 production factor for growth of roots  !kg CO2 kg-1 DM
! CO2SO  !CO2 production factor for growth of storage organs  !kg CO2 kg-1 DM
! CO2ST  !CO2 production factor for growth of stems  !kg CO2 kg-1 DM
! CO2STR  !CO2 production factor for growth of stem reserves  !kg CO2 kg-1 DM
! CRGCR  !Carbohydrate (CH2O) requirement for dry matter production  !kg CH2O kg-1 DM
! CTRANS  !Carbon losses at transplanting  !kg C ha-1
! GRT1  !Reduction in root weight at transplanting   !kg DM ha-1
! GST1  !Reduction in stem weight at transplanting  !kg DM ha-1
! KEEP  !Variable to temporarily store value of LDSTRS   !-
! DLEAF  !Control variable for start of leaf senescence by drought  !kg DM ha-1 d-1
! DROUT  !Control variable indicating drought/no drought  !-
! SUBROUTINES called: Oryza_PHENOL, Oryza_SUBLAI2, Oryza_SUBDD, Oryza_SUBCD, Oryza_SUBCBC, Oryza_SUBGRN    *
!                     GPPARSET, Oryza_SGPCDT                                 *
! LSTR  !Loss rate of stem reserves  !kg DM ha-1 d-1                                                                       *
! MNDVS  !Factor accounting for effect of DVS on maintenance respiration  !-
! RGCR  !Growth respiration rate of crop  !kg CO2 ha-1 d-1
! RMCR  !Maintenance respiration rate of crop  !kg CH2O ha-1 d-1
! RWLVG1  !Reduction in leaf weight at transplanting  !kg ha-1
! RWSTR1  !Reduction in stem reserve weight at transplanting  !kg ha-1
! TEFF  !Factor accounting for effect of temperature on respiration  !-
! TSHCKL  !Transplanting shock for leaf area development  !°Cd
! WLVGIT  !Temporary storage variable of WLVG  !kg ha-1
!
!
! Files included:      -                                               *
!                                                                      *
!----------------------------------------------------------------------*
          SUBROUTINE ORYZA1()
!
!===================================================================*
!     DECLARATION SECTION                                           *
!===================================================================*
         implicit none
!-----Formal parameters

!-----Local variables
      LOGICAL DLEAF, DROUT
!
      INTEGER IMX
      PARAMETER (IMX=40)

      REAL    CO2EFF
      real    efft
      real    CO2LV
      real    CO2ST
      real    CO2STR
      real    CO2SO
      real    CO2RT
      REAL    CRGCR
      real    CTRANS
      REAL    GRT1, GST1
      REAL    LSTR, KEEP
      REAL    MNDVS
      REAL    RMCR, RGCR
      REAL    RWLVG1, RWSTR1
      REAL    TEFF
      REAL    TSHCKL
      REAL    WLVGIT
      character  Err_string*400      ! Event message string
      
      g%WST    = g%WSTS + g%WSTR
      g%WLV    = g%WLVG + g%WLVD
      g%WAG    = g%WLVG + g%WST  + g%WSO
      g%WAGT   = g%WLV  + g%WST  + g%WSO
      g%TDRW   = g%WLV  + g%WST  + g%WSO + g%WRT
      DLEAF    = .FALSE.
      DROUT    = .FALSE.

      !!- Re-initialize rooting depth at day of transplanting
      IF (g%CROPSTA .EQ. 3) THEN
         g%ZRT = p%zrttr
      END IF

      IF (g%CROPSTA .GE. 1) THEN
           !!--Set DROUT when leaf expansion is reduced in the
           !!  vegetative growth phase (=> extra root growth)
           IF ((g%DVS.LT. 1.0).AND.(g%LESTRS.LT.1.)) THEN
              DROUT = .TRUE.
           ELSE
              DROUT = .FALSE.
           END IF

           !!-Computation of weather variables
           IF (g%CROPSTA .LE. 2) THEN
              g%TMPCOV = p%TMPSB
           ELSE
              g%TMPCOV = 0.
           END IF

           g%TMAX = g%tmmx+g%TMPCOV
           g%TMIN = g%tmmn
           g%TAV  = (g%TMIN+g%TMAX)/2.
           g%TAVD = (g%TMAX+g%TAV )/2.

           !!-Counter for days after emergence


           !!--Phenological development
           CALL Oryza_SUBDD()!Keep the arguments, different arguments in another calling
           CALL Oryza_SUBCD ()! Same arguments, By Xike 13/10/03
           CALL Oryza_PHENOL()! Same arguments
!
!!----------Effect of drought stress on development rate
!           IF (g%DVS.LT.1.0) THEN
!! BB: REMOVE THIS; IT CAN TAKE MORE THAN 1 YEAR TO COMPLETE A CROP CYCLE!!
!!              DVEW = LESTRS + (DVS*(1.-LESTRS))
!              g%DVEW = 1.
!           ELSE IF (g%DVS.GE.1.) THEN
!              g%DVEW = 1.
!           END IF
!           g%DVR = g%DVR*g%DVEW
!
!!----------CO2 concentration
           CO2EFF =(1.-EXP(-0.00305*g%co2-0.222))/
     :              (1.-EXP(-0.00305*p%co2ref-0.222))

           g%EFF = linear_interp_real(g%tavd,p%efft,p%eff,p%numeff)
     :              *CO2EFF

!!----------Leaf rolling under drought stress (only for photosynthesis)
           g%LAIROL = g%RLAI*(0.5*g%LRSTRS+0.5)
!
!!--------- Add specific stem area to leaf area
           g%SSGA = linear_interp_real(g%DVS,p%ssgat,p%ssga,p%numssga)
           g%SAI  = g%SSGA*g%WST
           g%ARLAI   = g%LAIROL+0.5*g%SAI
!
!!----------Intercepted solar radiation
           g%KDF = linear_interp_real(g%DVS,p%kdft,p%kdf,p%numkdf)
           g%REDF = linear_interp_real(g%TAVD,p%redft,p%redf,p%numredf)
           g%KNF = linear_interp_real(g%DVS,p%knft,p%knf,p%numknf)
!           g%NFLV = linear_interp_real(g%DVS,p%nflvt,p%nflv,p%numnflv)
!!----------Daily gross canopy CO2 assimilation (DTGA)
!           CALL Oryza_GPPARSET (g%CO2, g%KNF, g%NFLV, g%REDFT)
!!----------The value 2 in next argument list: accuracy for Gauss integration over canopy.
!!          If value=1 => 3-points Gauss over canopy (as in TOTASP); of value = 2 =>
!!          enhanced accuracy if required as detrmined within the subroutine TRY!
           CALL Oryza_SGPCDT() !different arguments, g%DTR,g%SCP,g%KDF,g%ALAI,g%DTGA
           g%PARI1 = g%RAPCDT/1.E6
           g%DPARI = g%RAPCDT/1.E6
           g%DPAR  = p%FRPAR*g%DTR/1.E6
!
!!----------Unrolling of ALAI again
           g%ARLAI  = g%RLAI+0.5*g%SAI
!
!!----------Effect of drought stress on DTGA
!
           g%DTGA  = g%DTGA*g%PCEW
!
!!----------Relative growth rates of shoots and roots
!!          Effect of drought stress on shoot-root partitioning
!!BB: Changed for aerobic rice according to SUCROS2
           g%FSH = linear_interp_real(g%DVS,p%fsht,p%fsh,p%numfsh)
           g%FSH  = divide(g%FSH*g%CPEW,1.+(g%CPEW-1.)*g%FSH,0.0)
           g%FRT = 1.-g%FSH
!!           IF (DVS.LT.1.) THEN
!!              FSH = FSH * LESTRS
!!              FRT = 1.-FSH
!!           END IF

!
!!----------Relative growth rates of shoot organs
           g%FLV = linear_interp_real(g%DVS,p%flvt,p%flv,p%numflv)
           g%FST = linear_interp_real(g%DVS,p%fstt,p%fst,p%numfst)
           g%FSO = linear_interp_real(g%DVS,p%fsot,p%fso,p%numfso)

!!----------Check sink limitation based on yesterday's growth rates
!!          and adapt partitioning stem-storage organ accordingly
           IF (g%GRAINS) THEN
              IF (g%GGR.GE.(g%PWRR-g%WRR)) THEN
               g%FSO = MAX(0.,divide(g%PWRR-g%WRR,g%GCR*g%FSH,0.0))
               g%FST = 1.-g%FSO-g%FLV
              END IF
           END IF

!!----------Loss rates of green leaves and stem reserves
           g%DRLV = linear_interp_real(g%DVS,p%drlvt,p%drlv,p%numdrlv)
!           g%NSLLV=linear_interp_real(g%DVS,p%nsllvt,p%nsllv,p%numnsllv)

           g%LLV  = g%NSLLV*g%WLVG*g%DRLV
!     dsg 160609 Rat grazing functionlaity for Peter Brown
           if (g%DVS.lt.1.0) then
             g%LLV_RAT = g%WLVG * divide(g%rat_graze_perc, 100.0, 0.0)
           else
             g%LLV_RAT = 0.0
           endif     
!! Stem loss starts only from flowering, by Xike, 23/10/03
           If(g%DVS.GE.1.0)then
            LSTR = divide(g%WSTR,p%TCLSTR,0.0)
           Else
            LSTR=0.0
           ENdif
!
!!----------Maintenance requirements
           TEFF = p%Q10**((g%TAV-p%TREF)/10.)
           MNDVS = g%WLVG/(g%WLVG+g%WLVD)
           RMCR  = (g%WLVG*p%MAINLV+g%WST*p%MAINST+g%WSO*p%MAINSO+
     :           g%WRT*p%MAINRT)*TEFF*MNDVS
!
!!----------Carbohydrate requirement for dry matter production (growth respiration)
           CRGCR = g%FSH*(p%CRGLV*g%FLV+p%CRGST*g%FST*(1.-p%FSTR)+
     :           p%CRGSTR*p%FSTR*g%FST+p%CRGSO*g%FSO)+p%CRGRT*g%FRT
!
!!----------Gross and net growth rate of crop (GCR, NGCR)
           g%GCR = divide(
     :       (g%DTGA*30./44.)-RMCR+(LSTR*p%LRSTR*p%FCSTR*30./12.)
     :       , CRGCR
     :       , 0.0)
           g%NGCR  = MAX(0.,g%GCR-LSTR*p%LRSTR*p%FCSTR*30./12.)
!
!!----------Set transplanting effect
           IF (g%CROPSTA .EQ. 3) THEN
             g%PLTR = p%NPLH*divide(p%NH,p%NPLSB,0.0)
           ELSE
             g%PLTR = 1.
           END IF
!
!!----------Growth rates of crop organs at transplanting
           RWLVG1 = g%WLVG*(1.-g%PLTR)
           GST1   = g%WSTS*(1.-g%PLTR)
           RWSTR1 = g%WSTR*(1.-g%PLTR)
           GRT1   = g%WRT *(1.-g%PLTR)
!
!!----------Growth rates of crop organs
           g%GRT    = g%GCR*g%FRT-GRT1
           g%GLV    = g%GCR*g%FSH*g%FLV-RWLVG1
!          dsg 160609 Rat grazing functionality.... old equation was g%RWLVG  = g%GLV-g%LLV
           g%RWLVG  = g%GLV-g%LLV-g%LLV_RAT
           g%GST    = g%GCR*g%FSH*g%FST*(1.-P%FSTR)-GST1
           g%GSTR   = g%GCR*g%FSH*g%FST*p%FSTR-RWSTR1
           g%RWSTR  = g%GSTR-LSTR
           g%GSO    = g%GCR*g%FSH*g%FSO
           IF (g%DVS.GT.0.95) THEN
!            incorporate rat-grazing functionality (g%rat_graze_perc will be zero if no grazing, so non-effecting)
           g%gr_rat_grazed = g%WSO*divide(g%rat_graze_perc, 100.0, 0.0)
             g%GSO = g%GSO - g%gr_rat_grazed
             g%GGR = g%GSO
            ELSE 
              g%GGR = 0.
           END IF
!
!!----------Growth rate of number of spikelets and grains
           CALL Oryza_SUBGRN () ! same arguments, CALL Oryza_SUBGRN (GCR,CROPSTA,LRSTRS,DVS,SF2,SF1,SPGF,TAV,TMAX, &
!                        !NSP,GNSP,GNGR,SPFERT,GRAINS)
!
!!--------- Leaf area growth (after calculation on leaf growth and loss rates!)
!
!!----------Temperature sum for leaf development
           CALL Oryza_SUBDD()
!
!!----------Specific leaf area
           IF (p%numsla .gt. 0) THEN
              g%SLA = linear_interp_real(g%DVS,p%slat,p%sla,p%numsla)
           ELSE
              g%SLA = p%ASLA + p%BSLA*EXP(p%CSLA*(g%DVS-p%DSLA))
              g%SLA = MIN(p%SLAMAX, g%SLA)
           END IF
!
!!----------Leaf area index growth
           CALL Oryza_SubLAI2()  !Same arguments, by Xike 13/10/03

!
!!----------Leaf death as caused by drought stress
           g%DLDR = 0.
           IF (g%LDSTRS.EQ.1.) THEN
               DLEAF = .FALSE.
               g%DLDRT = 0.
           END IF
           IF ((g%LDSTRS.LT.1.).AND.(.NOT.DLEAF)) THEN
              WLVGIT = g%WLVG
              DLEAF  = .TRUE.
              KEEP   = g%LDSTRS
           END IF
           IF (DLEAF) THEN
              IF (g%LDSTRS.LE.KEEP) THEN
                 g%DLDR=WLVGIT *
     :                  (1.-g%LDSTRS) -
     :                   g%DLDRT
                 KEEP  = g%LDSTRS
                 g%DLDRT = g%DLDR+g%DLDRT
              END IF
           END IF
!
!!----------Growth respiration of the crop (RGCR)
           CO2RT  = 44./12.*(P%CRGRT *12./30.-P%FCRT )
           CO2LV  = 44./12.*(P%CRGLV *12./30.-P%FCLV )
           CO2ST  = 44./12.*(P%CRGST *12./30.-P%FCST )
           CO2STR = 44./12.*(P%CRGSTR*12./30.-P%FCSTR)
           CO2SO  = 44./12.*(P%CRGSO *12./30.-P%FCSO )
!
           RGCR = (g%GRT+GRT1)*CO2RT + (g%GLV+RWLVG1)*CO2LV +
     :        (g%GST+GST1)*CO2ST + g%GSO*CO2SO+(g%GSTR+RWSTR1)*CO2STR+
     :                  (1.-P%LRSTR)*LSTR*P%FCSTR*44./12.
!
           CTRANS=RWLVG1*P%FCLV+GST1*P%FCST+RWSTR1*P%FCSTR+GRT1*P%FCRT
           g%RTNASS=((g%DTGA*30./44.-RMCR)*44./30.)-RGCR-
     :              (CTRANS*44./12.)
!
!!----------Carbon balance check
           g%CKCIN  = (g%WLVG+g%WLVD-p%WLVGI)*p%FCLV+(g%WSTS-p%WSTI)
     :           *p%FCST+g%WSTR*p%FCSTR
     :           +(g%WRT-p%WRTI)*p%FCRT+g%WSO*p%FCSO
           g%CKCFL  = g%TNASS*(12./44.)

           CALL Oryza_SUBCBC()
!
!!=======SET EXPORTED VARIABLES FOR SOIL BALANCE AT 0 BEFORE EMERGENCE
        ELSE IF (g%CROPSTA .EQ. 0) THEN
           g%RLAI    = 0.
           g%ARLAI   = 0.
           g%LAIROL = 0
        END IF
!!=======END OF SKIP WHOLE RATE CALCULATIONS BEFORE EMERGENCE
!
!!===================================================================*
!!     INTEGRATION SECTION                                           *
!!===================================================================*
!
!!=======SKIP WHOLE STATE UPDATE BEFORE EMERGENCE
         IF (g%CROPSTA .GE. 1) THEN
!
!!-----------Integrate rate variables
            g%PARCUM = g%PARCUM + g%DPARI
            g%PARCM1 = g%PARCM1 + g%PARI1
            g%TS     = g%TS + g%HU
            g%TSLV   = g%TSLV + g%HULV
            g%DVS    = g%DVS + g%DVR
            g%WLVG   = g%WLVG + (g%RWLVG-g%DLDR)
            g%WLVD   = g%WLVD + (g%LLV+g%DLDR)  ! dry weight of dead leaves
            g%WSTS   = g%WSTS + g%GST
            g%WSTR   = g%WSTR + g%RWSTR
            g%WSO    = g%WSO + g%GSO
            g%WRT    = g%WRT + g%GRT
            g%WRR    = g%WRR + g%GGR
            g%NGR    = g%NGR + g%GNGR
            g%NSP    = g%NSP + g%GNSP

            g%TNASS  = g%TNASS + g%RTNASS
!!-----------Calculate sums of states
            g%WST    = g%WSTS + g%WSTR
            g%WLV    = g%WLVG + g%WLVD
            g%WAG    = g%WLVG + g%WST  + g%WSO
            g%WAGT   = g%WLV  + g%WST  + g%WSO
            g%TDRW   = g%WLV  + g%WST  + g%WSO + g%WRT
!
            g%PWRR   = g%NGR*p%WGRMX
            g%NGRM2  = g%NGR/10000.
            g%NSPM2  = g%NSP/10000.

!!-----------Leaf area index and total area index (leaves + stems)
            g%RLAI    = g%RLAI + g%GLAI
            g%ARLAI   = g%RLAI+0.5*g%SAI

!
!!-----------Root length
            IF ((.NOT.DROUT).AND.(g%ZRT.LE.p%ZRTMCW)) THEN
               g%ZRTM = MIN(p%ZRTMCW,p%ZRTMS,g%TKLT)
            ELSE IF ((.NOT.DROUT).AND.(g%ZRT.GT.p%ZRTMCW)) THEN
               g%ZRTM = MIN(g%ZRT,p%ZRTMS,g%TKLT)
            ELSE IF (DROUT) THEN
               g%ZRTM = MIN(p%ZRTMCD,p%ZRTMS,g%TKLT)
            END IF
            g%ZRT    = g%ZRT + p%GZRT
            g%ZRT    = MIN(g%ZRT,g%ZRTM)
!
!!===========Checks on simulation run
!!-----------If biomass is negative: set at 0 and abort simulation
            IF (g%WSO.LT.-5..OR.g%WLVG.LT.-5..OR.g%WST.LT.-5.) THEN
!               call fatal_error(err_user,
!     :               'Negative biomass in oryza1')
 

              Write (Err_string,*) 'Negative biomass CROP DEATH'
   	      call Write_string (Err_string)
              call oryza_end_crop ()

               IF (g%WSO.LT.0.) g%WSO = 0.
               IF (g%WST.LT.0.) g%WST = 0.
               IF (g%WLVG.LT.0.) g%WLVG = 0.
            END IF
!
!!-----------The following only in main field
            IF (g%CROPSTA .GE. 4) THEN
!!           Check if lower limit dead leaves is reached
               IF (g%LDSTRS.LE.0.) THEN
               call fatal_error(err_user,
     :           'Soil dryer than lower limit when dead' //
     :           'LDSTRS <= 0')
               END IF
!!-----------End if only in main field
            END IF
!          !
!!========END OF SKIP WHOLE RATE CALCULATIONS BEFORE EMERGENCE
         END IF
!
!!Leave this section for while until Peter's suggestion, By Xike, 13/10/03
!!===================================================================*
!!     TERMINAL SECTION                                              *
!!!===================================================================*
!!      ELSE IF (ITASK.EQ.4) THEN
!!!        Terminal calculations
!!!        Terminal output
!!         CALL OPSTOR ('WRR14', WRR14)
!!         CALL OPSTOR ('WSO', WSO)
!!         CALL OPSTOR ('WAGT', WAGT)
!!         CALL OPSTOR ('WLVD', WLVD)
!      END IF
!!

      RETURN
      END Subroutine

!----------------------------------------------------------------------!
!  SUBROUTINE Oryza_WSTRESS                                                  !
!  Used in ORYZA2000 model version 1.0                                 !
!  Date   : December 2001                                              !
!  Author : B.A.M. Bouman                                              !
!  Version: 1.0 (Based on earlier versions of DSTRES)                  !
!  Version: Redistribution of transpiration uptake per soil layer.     !
!           Avarage stress factors over all soil layers                !
!           Calculations based on Wopereis Oryza_ET al (1996a)               !
!                                                                      !
!  Purpose: Calculate actual transpiration of a crop, and the  effects !
!          of water stress on growth and development of rice.          !
!                                                                      !
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      !
! name   type meaning (unit)                                     class !
! ----   ---- ---------------                                    ----- !
! ITASK   I4  Task that subroutine should perform (-)               I  !
! DELT    R4  Time step of integration (d)                          T  !
! OUTPUT  R4  Flag to indicate if output should be done (-)         I  !
! IUNITD  I4  Unit that can be used for input files (-)             I  !
! IUNITL  I4  Unit number for log file messages (-)                 I  !
! FILEI1  C*  Name of file with input model data (-)                I  !
! TRC     R4  Potential transpiration rate (mm d-1)                 I  !
! ZRT     R4  Rooting depth (m)                                     I  !
! TKL     R4  Array of thicknesses soil layers (m)                  I  !
! NL      I4  Number of soil layers (-)                             I  !
! CROPSTA I4  Crop stage (-)                                        I  !
! WCWP    R4  Array of water content at wilting point/layer (m3 m-3)I  !
! WCAD    R4  Array of water content air dry/ layer (m3 m-3)        I  !

! TRW     R4  Actual transpiration rate (mm)                        O  !
! TRWL    R4  Array of actual transpiration rate/layer (mm d-1)     O  !
! LRSTRS  R4  Stress factor for rolling of leaves (-)               O  !
! LDSTRS  R4  Stress factor for dead leaves (-)                     O  !
! LESTRS  R4  Stress factor for expansion of leaves (-)             O  !
! PCEW    R4  Stress factor for CO2 assimilation (-)                O  !
! LD  !Array of drought factors accelerating leaf death, per soil layer  !-                                                                     !
! LE  !Array of drought stress factors reducing leaf expansion, per soil layer  !-
! LR  !Array of drought stress factors causing leaf rolling, per soil layer  !-

! TRR  !Array of relative transpiration ratios, per soil layer  !mm d-1
! WLA  !Array of total amount of water that can be extracted by roots in each soil layer  !mm
!
! Files included: -                                                    !
!                                                                      !
!----------------------------------------------------------------------!
      SUBROUTINE Oryza_WSTRESS ()


      IMPLICIT NONE

!-----Formal parameters

!-----Local variables
      INTEGER    I, J, num_layers
      REAL LE(max_layer)
      REAL LD(max_layer)
      REAL TRR(max_layer)
      REAL WLA(max_layer)
      REAL LR(max_layer)
      REAL LIMIT

      IF (g%CROPSTA .EQ. 4) THEN
         !!- Only stress in main field after day of transplanting
         g%TRRM = g%TRC/(g%ZRT+1.0E-10)

         g%TRW  = 0.
         g%ZLL  = 0.
         g%LRAV = 0.
         g%LEAV = 0.
         g%LDAV = 0.

         num_layers = count_of_real_vals(p%tkl,max_layer)
         num_layers = max(1,num_layers)
         DO I = 1,num_layers

            IF (g%WCL(I).GE.p%WCFC(I)) THEN
                  g%FACT(I)= MAX(0.,MIN(1.,(p%WCST(I)-g%WCL(I))/
     :                          (p%WCST(I)-p%WCFC(I))))
                  g%MSUC(I) = 10.**(g%FACT(I)*2.0)
                  IF (g%WCL(I).GE.p%WCST(I)) g%MSUC(I) = 0.
            ELSEIF(g%WCL(I).GE.p%WCWP(I).AND.
     :              g%WCL(I).LT.p%WCFC(I)) THEN
                  g%FACT(I) = MAX(0.,MIN(1.,(g%WCL(I)-p%WCWP(I))
     :                 /(p%WCFC(I)-p%WCWP(I))))
                  g%MSUC(I) = 10.**(4.2-g%FACT(I)*2.2)
            ELSEIF (g%WCL(I).LT.p%WCWP(I)) THEN
                  g%FACT(I) = MAX(0.,MIN(1.,(g%WCL(I)-p%WCAD(I))/
     :                           (p%WCWP(I)-p%WCAD(I))))
                  g%MSUC(I) = 10.**(7.0-g%FACT(I)*2.8)
            ENDIF

            !! Note: MSKPA(I) is matrix moisture suction in kPa!
            g%MSKPA(I) = (g%MSUC(I)/10.)

            !!-----------Root length in each soil layer
            g%ZRTL(I)  = MIN(p%TKL(I)/1000,MAX((g%ZRT-g%ZLL/1000),0.0))

!        dsg 290507  added
!            !!-----------Root mass in each soil layer
            g%root_weight_layer(I) = g%wrt * divide(g%ZRTL(I),g%ZRT,0.0)
            
!        dsg 290507  added
!            !!-----------Root length (mm/mm2)in each soil layer
              g%root_length_layer(I) = g%root_weight_layer(I) * 
     :                divide(p%specific_root_length,10000000.0,0.0)
     
!        dsg 290507  added
!            !!-----------RLV (mm/mm3)in each soil layer
            g%rlv(I)= divide(g%root_length_layer(I),p%TKL(I),0.0)
            !!-----------Leaf-rolling factor
            LR(I) = (LOG10(g%MSKPA(I)+TINY)-LOG10(p%LLLS))
     :                          /(LOG10(p%ULLS)-LOG10(p%LLLS))
            If(LR(I).lt.0.0) LR(I)=0.0
            If(LR(I).gt.1.0) LR(I)=1.0
            g%LRAV  = g%LRAV+(g%ZRTL(I)/(g%ZRT+TINY))*LR(I)

            !!-----------Relative leaf expansion rate factor
            LE(I) = (LOG10(g%MSKPA(I)+TINY)-LOG10(p%LLLE))
     :                          /(LOG10(p%ULLE)-LOG10(p%LLLE))
            If(LE(I).lt.0.0) LE(I)=0.0
            If(LE(I).gt.1.0) LE(I)=1.0
            g%LEAV  = g%LEAV+(g%ZRTL(I)/(g%ZRT+TINY))*LE(I)

            !!-----------Relative death rate factor
            LD(I) = (LOG10(g%MSKPA(I)+TINY)-LOG10(p%LLDL))
     :                   /(LOG10(p%ULDL)-LOG10(p%LLDL))

            If(LD(I).lt.0.0) LD(I)=0.0
            If(LD(I).gt.1.0) LD(I)=1.0
            g%LDAV  = g%LDAV+(g%ZRTL(I)/(g%ZRT+TINY))*LD(I)

            !!- Relative transpiration ratio (actual/potential)
            IF (g%MSKPA(I) .GE. 10000.) THEN
               TRR(I) = 0.
            ELSE
               IF (p%SWIRTR .EQ. ET_EXPONENTIAL) THEN
                  ! Woperis, p91
                  TRR(I) = (LOG10(g%MSKPA(I)+TINY)-LOG10(p%LLRT))
     :                              /(LOG10(p%ULRT)-LOG10(p%LLRT))
                  If(TRR(I).lt.0.0) TRR(I)=0.0
                  If(TRR(I).gt.1.0) TRR(I)=1.0
               ELSEIF (p%SWIRTR .EQ. ET_LINEAR) THEN
                  ! Tanner & sinclair, p91
                  TRR(I)  = 2./(1.+EXP(0.003297*g%MSKPA(I)))
               ELSE
                  call fatal_error(err_user, 'ET method wrong??')
               END IF
            END IF
            If(TRR(I).lt.0.0) TRR(I)=0.0
            If(TRR(I).gt.1.0) TRR(I)=1.0

            WLA(I)=MAX(0.0,(g%WCL(I)-p%WCWP(I))*g%ZRTL(I)*1000.)
            g%TRWL(I) = MIN(TRR(I)*g%ZRTL(I)*g%TRRM,WLA(I))
            g%TRW     = g%TRW + g%TRWL(I)
            g%ZLL     = g%ZLL+p%TKL(I)
         END DO

         if (p%uptake_source .eq. 'calc') then
         !!-----Compensation of water extraction from soil layers if drought stress occurs
         !!     Take water from soil layer that has a surplus, starting from top.
         DO I = 1, num_layers
            IF (g%TRW .LT. g%TRC) THEN
               IF (TRR(I).GE.1 .AND.g%TRWL(I).LT.WLA(I)) THEN
                  g%TRWL(I) = MIN(WLA(I),(g%TRWL(I)+
     :                       (g%TRC-g%TRW)))
               END IF
               g%TRW = 0.
               DO J = 1,num_layers
                 g%TRW = g%TRW + g%TRWL(J)
               END DO
            END IF
         END DO

         elseif (p%uptake_source .eq. 'apsim' .or.
     :           p%uptake_source .eq. 'swim3' ) then
            
           !    dsg  230507   get the water uptake from apswim
           call get_real_array(unknown_module, 
     :                        'uptake_water_' // trim(p%crop_type)
     :                        , max_layer
     :                        , '(mm)'
     :                        , g%TRWL, num_layers
     :                        , 0.0, 1000.0)
           g%TRW     = 0
           DO I = 1, num_layers
              g%TRW     = g%TRW + g%TRWL(I)
           END DO
         else
            ! notreached
         endif
         g%PCEW   = g%TRW/g%TRC

         !!-----Set stress factors as average over all layers: DEFAULT
         g%LRSTRS = g%LRAV
         g%LDSTRS = g%LDAV
         g%LESTRS = g%LEAV
      ELSE
        !!-------If crop is not in the main field, set all stres factors at 1.
        g%PCEW   = 1.
        g%LRSTRS = 1.
        g%LDSTRS = 1.
        g%LESTRS = 1.
      END IF

      g%CPEW = g%LESTRS

      RETURN

      END Subroutine


!----------------------------------------------------------------------!
!  SUBROUTINE Oryza_WNOSTRESS                                                !
!  Used in ORYZA2000 model version 1.0                                 !
!  Date  : December 2001                                               !
!  Author: B.A.M. Bouman                                               !
!                                                                      !
!  Purpose: Invoked when production environment is POTENTIAL.          !
!           Sets actual transpiration of a crop at zero, and sets      !
!           effects of water stress on growth and development of rice  !
!           at unity.                                                  !
!                                                                      !
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      !
! name   type meaning (unit)                                     class !
! ----   ---- ---------------                                    ----- !
! NL      I4  Number of soil layers (-)                             I  !
! TRW     R4  Actual transpiration rate (mm d-1)                    O  !
! TRWL    R4  Array of actual transpiration rate/layer (mm d-1)     O  !
! LRSTRS  R4  Stress factor for rolling of leaves (-)               O  !
! LDSTRS  R4  Stress factor for accelerating leaf death (-)         O  !
! LESTRS  R4  Stress factor for reducing expansion of leaves (-)    O  !
! PCEW    R4  Stress factor for CO2 assimilation (-)                O  !
!                                                                      !
!----------------------------------------------------------------------!
      SUBROUTINE Oryza_WNOSTRESS ()
      IMPLICIT NONE

!-----Local variables
      INTEGER I,num_layers

      g%TRW    = 0.
      g%LRSTRS = 1.
      g%LDSTRS = 1.
      g%LESTRS = 1.
      g%CPEW   = 1.
      g%PCEW   = 1.

      num_layers = count_of_real_vals(p%tkl,max_layer)
      num_layers = max(1,num_layers)
      DO I=1,num_layers
        g%TRWL(I) = 0.
      END DO

      RETURN
      END Subroutine

!----------------------------------------------------------------------!
! SUBROUTINE Oryza_NCROP                                                     !
! Authors:                                                             !
! Date   : December 2001, Version: 1                                    !
! Purpose: This subroutine calculates the nitrogen dynamics in a rice  !
!          crop and the effects on growth and development              !
!                                                                      !
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      !
! name   type meaning                                     units  class !
!      REAL    ANCR  !Amount of N in crop (live and dead material)  !kg N ha-1
!      REAL    ANCRF  !Amount of N in crop till flowering  !kg N ha-1
!      REAL    ANCRPT  !Potential amount of N in crop  !kg N ha-1
!      REAL    ANLD  !Amount of N in dead leaves  !kg N ha-1
!      REAL    ANLV  !Amount of N in leaves  !kg N ha-1
!      REAL    ANLVA  !Amount of N in leaves till flowering  !kg N ha-1
!      REAL    ANSO  !Amount of N in storage organs  !kg N ha-1
!      REAL    ANST  !Amount of N in stems  !kg N ha-1
!      REAL    ANSTA  !Amount of N in stems till flowering  !kg N ha-1
! ----   ---- -------                                     -----  ----- !
! ITASK   I4  Task that subroutine should perform (-)               I  !
! IUNITD  I4  Unit that can be used for input files (-)             I  !
! IUNITL  I4  Unit number for log file messages (-)                 I  !
! FILEI1  C*  Name of file with crop data (-)                       I  !
! DELT    R4  Time step of integration (d)                          I  !
! TIME    R4  Time of simulation (d)                                I  !
! OUTPUT  R4  Flag to indicate if output should be done (-)         I  !
! TERMNL  L4  Flag to indicate if simulation is to stop (-)         I  !
! DVS     R4  Development stage of the crop (-)                     I  !
! LLV     R4  Loss rate of leaves caused by senescence (kg ha-1 d-1)I  !
! DLDR    R4  Loss rate of leaves caused by drought (kg ha-1 d-1)   I  !
! WLVG    R4  Dry weight of green leaves (kg ha-1)                  I  !
! WST     R4  Dry weight of stems (kg ha-1)                         I  !
! WSO     R4  Dry weight of storage organs (kg ha-1)                I  !
! GSO     R4  Growth rate of storage organs (kg ha-1 d-1)           I  !
! GST     R4  Growth rate of stems (kg ha-1 d-1)                    I  !
! GLV     R4  Growth rate of leaves (kg ha-1 d-1)                   I  !
! PLTR    R4  Intermediate variable for planting density (-)        I  !
! LAI     R4  Leaf area index (ha ha-1)                             I  !
! CROPSTA I4  Crop stage (-)                                        I  !
! TNSOIL  R4  Soil-N available for crop uptake (kg N ha-1 d-1)      I  !
! NACR    R4  Actual N uptake rate by the crop (kg ha-1 d-1)        O  !
! NFLV    R4  Nitrogen fraction in the leaves (g N m-2 leaf)        O  !
! NSLLV   R4  Stress factor for leaf death caused by N stress  (-)  O  !
! RNSTRS  R4  Decrease factor for RGRL caused by N stress (-)       O  !
! FNSO  !Fraction of N in storage organs  !kg N kg-1 DM                                                                     !
! ATN  !Total available N for translocation from leaves, stems, and roots  !kg N ha-1
! ATNLV  !Total available N for translocation from leaves  !kg N ha-1
! ATNRT  !Total available N for translocation from roots   !kg N ha-1
! ATNST  !Total available N for translocation from stems  !kg N ha-1
! FNST  !Fraction of N in stems  !kg N kg-1 DM
! NACRS  !Cumulative amount of nitrogen taken up by crop  !kg N ha-1
! NALV  !Actual nitrogen uptake rate by leaves  !kg N ha-1 d-1
! NALVS   !Cumulative amount of nitrogen taken up by leaves  !kg N ha-1
! NASO  !Actual nitrogen uptake rate by storage organs  !kg N ha-1 d-1
! NASOS  !Cumulative amount of nitrogen taken up by storage organs  !kg N ha-1
! NAST  !Actual nitrogen uptake rate by stems  !kg N ha-1 d-1
! NASTS  !Cumulative amount of nitrogen taken up by stems  !kg N ha-1
! NCHCK  !Balance of nitrogen uptake  !kg N ha-1
! NDEMC  !Potential daily N demand by crop  !kg N ha-1 d-1
! NDEML  !Potential daily N demand by leaves  !kg N ha-1 d-1
! NDEMS  !Potential daily N demand by stems  !kg N ha-1 d-1
! NDEMSN   !Minimum daily N demand by storage organs  !kg N ha-1 d-1
! NDEMSX  !Potential daily N demand by storage organs  !kg N ha-1 d-1
! NLDLV  !N loss rate because of death of leaves  !kg N ha-1 d-1
! NLV  !Daily net flow rate of N to the leaves  !kg N ha-1 d-1
! NLVAN  !Daily net flow rate of N to the leaves before flowering  !kg N ha-1 d-1
! NSHKLV  !Correction for leaf-N loss because of transplanting  !-
! NSHKST  !Correction for stem-N loss because of transplanting  !-
! NSO  !Net flow rate of N to storage organs  !kg N ha-1 d-1
! NST  !Net flow rate of N to stems  !kg N ha-1 d-1
! NSTAN  !Net flow rate of N to stems before flowering  !kg N ha-1 d-1
! NSTRES  !Ratio of maximum over actual amount of N in crop  !-
! NTLV  !Actual N translocation rate to storage organs from leaves  !kg N ha-1 d-1
! NTRT  !Actual N translocation rate to storage organs from roots  !kg N ha-1 d-1
! NTRTS  !Amount of N translocated from roots to storage organs  !kg N ha-1
! NTSO  !Actual N translocation rate to storage organs from leaves, stems and roots  !kg N ha-1 d-1
! NTST  !Actual N translocation rate to storage organs from stems  !kg N ha-1 d-1
!
!
! Subroutine called: Oryza_SUBNBC                                            !
!                                                                      !
!----------------------------------------------------------------------*
      SUBROUTINE Oryza_NCROP ()


      IMPLICIT NONE

!     Formal parameters

!     Local parameters
      INTEGER IMX
      PARAMETER (IMX=40)
      REAL NFLVP,NUPP
      REAL N_RAT_LOSS  ! dsg 160609 Rat grazing functionality
      REAL ATNLV, ATNST, ATNRT
      REAL ANST, ANCRPT
      REAL NLDLV, NSHKLV, NSHKST
      REAL NALV, NAST, NASO
      REAL NDEML,NDEMS,NDEMSX,NDEMC,NDEMSN
      REAL ATN,NTSO,NTLV,NTST,NTRT,NLV,NST,NSO,NSTAN, NLVAN
      REAL NSTRES

      ntrt = 0.0
      NALV = 0.0
      NAST = 0.0
      NASO = 0.0
      NLVAN= 0.0
      NSTAN= 0.0
      NSO  = 0.0
      NLV  = 0.0
      NST  = 0.0
      NLDLV= 0.0
!!=====Rate calculations
      g%NMINSO=linear_interp_real(g%ANCRF,p%nminsot,p%nminso,
     :                            p%numnminso)

      g%NMAXL=linear_interp_real(g%DVS,p%nmaxlt,p%nmaxl,p%numnmaxl)
      g%NMINL=linear_interp_real(g%DVS,p%nminlt,p%nminl,p%numnminl)
!        NFLVP=linear_interp_real(g%DVS,p%nflv,p%nflvt,p%numnflv)

!!====== Only calculations after sowing
      IF (g%CROPSTA .GE. 4) THEN
          !!       Potential leaf N content (on LAI basis)
          NFLVP=linear_interp_real(g%DVS,p%nflvt,p%nflv,p%numnflv)

          !!========== Calculate (potential) N demand of crop organs
          !!           Maximum N demand of leaves

          NDEML  = (g%NMAXL*(g%WLVG+g%GLV)-g%ANLV)

          IF (NDEML .LT. 0.) NDEML = 0.
          !!           Maximum N demand of stems
          NDEMS  = (g%NMAXL*0.5*(g%WST+g%GST)-g%ANST)
          IF (NDEMS .LT. 0.) NDEMS = 0.
          !!           Maximum N demand of storage organs
          NDEMSX = p%NMAXSO*g%GSO
          IF (NDEMSX .LT. 0.) NDEMSX = 0.
          !!           Minimum nitrogen demand of storage organs
          NDEMSN = g%NMINSO*g%GSO
          IF (NDEMSN .LT. 0.) NDEMSN = 0.
          !!========== Calculate translocation of N from organs, in kg/ha/d
          !!           It is assumed that potential demand by storage organ is first met by translocation
          !!           No translocation before DVS = 0.95
          IF (g%DVS .LT. 0.95) THEN
              ATNLV = 0.
              ATNST = 0.
              ATNRT = 0.0
              ATN   = 0.
              NTSO = 0.
          ELSE
              !! Maximum translocation amount from leaves and stems
              ATNLV = MAX(0., g%ANLV-g%WLVG*p%RFNLV)
              ATNST = MAX(0., g%ANST-g%WST*p%RFNST)
              !! Maximum translocation amount from roots as fraction of that of shoot
              ATNRT = (ATNLV+ATNST)*p%FNTRT
              ATN   = ATNLV+ATNST+ATNRT
              !! Daily translocation is total pool divided by time constant, 10 days!
              NTSO = divide(ATN,p%TCNTRF,1.0)
              !! Translocation is limited between minimum (NDEMSN) and maximum (NDEMSX)
              IF(NTSO .gt.NDEMSX) NTSO=NDEMSX
              IF(NTSO .lt.NDEMSN) NTSO=NDEMSN
          END IF

          !!---------- Actual N translocation rates from plant organs, in kg/ha/d
          NTLV  = NTSO*divide(ATNLV,ATN,1.0)
          NTST  = NTSO*divide(ATNST,ATN,1.0)
          NTRT  = NTSO*divide(ATNRT,ATN,1.0)

          != Calculate nitrogen uptake
          !! Available N uptake is mimimum of soil supply and maximum crop uptake
          NUPP = MIN(p%NMAXUP, g%TNSOIL)  !XXX ensure tnsoil is calc form soiln2

          IF (NUPP .LT. 0.) NUPP = 0.
          !! Sum total maximum uptake rates from leaves, stems and storage organs
          NDEMC  = (NDEML+NTLV)+(NDEMS+NTST)+(NDEMSX-NTSO)
          !! Actual uptake per plant organ is minimum of availability and demand
          NALV=MAX(0.,MIN(NDEML+NTLV,
     :                    NUPP*divide(NDEML+NTLV,
     :                                NDEMC,1.0)))
          NAST=MAX(0.,MIN(NDEMS+NTST,
     :                    NUPP*divide(NDEMS+NTST,
     :                                NDEMC,1.0)))
          NASO=MAX(0.,MIN(NDEMSX-NTSO,
     :                    NUPP*divide(NDEMSX-NTSO,
     :                                NDEMC,1.0)))

          !! Total uptake by crop from the soil
          g%NACR = NALV+NAST+NASO
          !!=Calculate net N flows to plant organs (daily rates)
          !! Transplanting shock: remove N
          NSHKLV =g%ANLV*(1.-g%PLTR)
          NSHKST =g%ANST*(1.-g%PLTR)
          !! Loss of N from leaves by leaf death
          NLDLV = (g%LLV+g%DLDR)*p%RFNLV
          !! Loss of N from leaves by rat feasting
          N_RAT_LOSS = (g%LLV_RAT)*p%RFNLV
          !!- Net flow to stems and leaves
          NLV = NALV-NTLV-NLDLV-NSHKLV-N_RAT_LOSS
          NST = NAST-NTST-NSHKST

          !!-Net N flow to storage organ
          NSO = NTSO+NASO

          !!--Net flow to stems and leaves before flowering
          IF (g%DVS.LT. 1.) THEN
             NSTAN =NST
             NLVAN =NLV
          ELSE
             NSTAN = 0.
             NLVAN = 0.
          END IF
      END IF !!=End if statement for CROPSTA GT 4

      !!------- N amount in plant organs
      g%ANSO  =g%ANSO +NSO
      g%ANLV  =g%ANLV +NLV
      g%ANST  =g%ANST +NST
      g%ANLD  =g%ANLD +NLDLV
      g%ANCR  =g%ANSO+g%ANLV+g%ANLD+g%ANST

      !!------- N amount in plant organs before flowering
      g%ANLVA =g%ANLVA + NLVAN
      g%ANSTA =g%ANSTA + NSTAN
      g%ANCRF =g%ANSTA + g%ANLVA

      !!------- Total N uptake from soil
      g%NALVS = g%NALVS+ NALV
      g%NASTS = g%NASTS+ NAST
      g%NASOS = g%NASOS+ NASO
      g%NACRS = g%NALVS + g%NASTS + g%NASOS

      !!------- Total N supply by translocation from roots
      g%NTRTS = g%NTRTS + NTRT

      !!------- Nitrogen balance check
      CALL Oryza_SUBNBC (g%ANCR, g%NACRS+g%NTRTS)

      !!======= Calculate N contents and N stress factors (only if in main field)
      IF (g%CROPSTA .LT. 4) THEN
          g%FNLV   = p%FNLVI
          g%FNST   = 0.5*p%FNLVI
          g%FNSO   = 0.
          g%NFLV   = p%NFLVI
          g%NSLLV  = 1.
          g%RNSTRS = 1.
      ELSE
          !!---------- Fraction N in plant organs
          g%FNLV  =divide(g%ANLV,g%WLVG,1.0)
          g%FNst  =divide(g%anst,g%wst,1.0)
          g%FNso  =divide(g%anso,g%wso,1.0)

          !! Leaf N content in g N/m2 leaf

          IF (g%RLAI .EQ. 0.) THEN
            g%NFLV = p%NFLVI
          ELSE
            IF (g%RLAI .LT.1.0 .AND. g%DVS .LT. 1.0) THEN
                g%NFLV = divide(g%FNLV, g%NMAXL, 1.0)*NFLVP
            ELSE
                g%NFLV = divide(g%ANLV,10.0*g%RLAI,1.0)
            END IF
          END IF

          !!---------- Set N stress factor for leaf death
          ANCRPT =g%WLVG*g%NMAXL+g%WST*g%NMAXL*0.5+g%WSO*p%NMAXSO
          IF (g%ANCR .EQ. 0.) THEN
             NSTRES = 2.0
          ELSE
             NSTRES = divide(ANCRPT,g%ANCR,0.0)
          END IF
          IF (NSTRES .LT. 1.) NSTRES = 1.
          IF (NSTRES .GT. 2.) NSTRES = 2.
          g%NSLLV=linear_interp_real(nstres,p%nsllvt,p%nsllv,
     :                               p%numnsllv)
          !!-------- Set N stress factor for RGRL

          g%RNSTRS = divide(g%FNLV-0.9*g%NMAXL,
     :                      g%NMAXL-0.9*g%NMAXL,
     :                      0.0)


          IF (g%RNSTRS .GT. 1.) g%RNSTRS = 1.
          IF (g%RNSTRS .LT. 0.) g%RNSTRS = 0.


        !!======= End IF statement for main field
        END IF

      RETURN
      END Subroutine


!----------------------------------------------------------------------*
! SUBROUTINE Oryza_NNOSTRESS                                                 *
! Authors: Bas Bouman                                                  *
! Date   : Jan-2001, Version: 1.0                                      *
! Purpose: This subroutine sets nitrogen stress factors on crop growth *
!          to unity, and gets leaf N content as function of DVS or     *
!          from observed values in the experiment data file.           *
!          A programming 'trick' using the function INTGR2 is used to  *
!          get DVS-interpolated values for leaf N content before and   *
!          after the first and last day of observation, respectively.  *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning                                     units  class *
! ----   ---- -------                                     -----  ----- *
! DELT    R4  Time step of integration (d)                          I  *
! IUNITD  I4  Unit that can be used for input files (-)             I  *
! IUNITL  I4  Unit number for log file messages (-)                 I  *
! ITASK   I4  Task that subroutine should perform (-)               I  *
! FILEI1  C*  Name of file with crop input data (-)                 I  *
! FILEIT  C*  Name of file with experimental data (-)               I  *
! CROPSTA I4  Crop stage (-)                                        I  *
! DVS     R4  Development stage (-)                                 I  *
! NFLV    R4  Nitrogen fraction in the leaves (g N m-2 leaf)        O  *
! NSLLV   R4  N stress factor on leaf death (-)                     O  *
! RNSTRS  R4  N stress factor on relative leaf growth (-)           O  *
!                                                                      *
!----------------------------------------------------------------------*
      SUBROUTINE Oryza_NNOSTRESS()

      IMPLICIT NONE


!-------Calculate nitrogen content after emergence/sowing
        IF (g%CROPSTA .GE. 1) THEN
!          Read NFLV as function of development state
!           g%NFLV1 = LINT2('NFLVTB',g%NFLVTB,ILNFLV,g%DVS)
!          This is a programming 'trick' to enable forcing of observed
!          NFLV as function of day-of-observation. Below the first observation
!          day, NFLV1 is used;l between first and last observation day, interpolated
!          observed values are used; after last observation day, NFLV1 is used again
!          Forcing is determined by the variable NFLV_FRC in the experiment data file.
           g%NFLV=linear_interp_real(g%DVS,p%nflvt,p%nflv,p%numnflv)
!           g%NFLV   = INTGR2(0., g%NFLV1, p%DELT, FILEIT, 'NFLV')
           g%rnstrs=1.0
       END IF


      RETURN
      END Subroutine


!----------------------------------------------------------------------*
!  SUBROUTINE Oryza_SUBNBC                                                   *
!  Purpose: This subroutine checks the Crop Nitrogen Balance           *
!           and stops the simulation if the difference between         *
!           CHKIN and CHKFL exceeds 0.1 %                              *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! CHKIN   R4  Accumulated N in the crop (kg N ha-1)                 I  *
! CHKFL   R4  Sum of N supplied by soil and roots (kg N ha-1)       I  *
! TIME    R4  Time of simulation (d)                                T  *
! NBCHK   R4  Nitrogen balance check, relative value to the sums of    *
!             CHKIN and CHKFL (-)                                   O  *
! TERMNL  R4  Flag to indicate if simulation is to stop (-)         O  *
!                                                                      *
!  FILE usage : none                                                   *
!----------------------------------------------------------------------*
      SUBROUTINE Oryza_SUBNBC(NCHKIN, NCKCFL)
      IMPLICIT NONE
      Real NCKCFL,NCHKIN

!     Local Variables:
      character string*200
      real   nbchk

      NBCHK = 2.0*(NCHKIN-NCKCFL)/(NCHKIN+NCKCFL+1.E-10)
!
      IF (ABS(NBCHK).GT.0.001) THEN
         WRITE (string,'(A,3(A,A,F8.2))')
     :     '* * * Error in Nitrogen Balance, please check * * *',
     :   new_line, ' NBCHK=',NBCHK,
     :   new_line, ' NCKCFL=',NCKCFL,
     :   new_line, ' NCHKIN=',NCHKIN
!         call fatal_error(err_user, string)
      END IF


      RETURN
      END Subroutine


* ====================================================================
       subroutine oryza_set_my_variable (Variable_name)
* ====================================================================
      implicit none


*+  Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name to search for

*+  Purpose
*     Set one of our variables altered by some other module

*+  Mission Statement
*     Set Variable as Requested

*+  Changes
*    070696  nih changed repsond2set call to collect call

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'oryza_set_my_variable')

*+  Local Variables
      integer numvals                  ! number of values returned
!      integer num_layers
*- Implementation Section ----------------------------------
      call push_routine (myname)

       if (Variable_name .eq. 'tmda') then

         call collect_real_var (
     :                variable_name        ! variable name
     :               ,'(oC)'              ! units
     :               ,g%tmda               ! variable
     :               ,numvals              ! number of elements returned
     :               ,0.0                  ! lower limit for bound check
     :               ,100.0)                 ! upper limit for bound check

       elseif (Variable_name .eq. 'etd') then

         call collect_real_var (
     :                variable_name        ! variable name
     :               ,'(mm/day)'              ! units
     :               ,g%etd               ! variable
     :               ,numvals              ! number of elements returned
     :               ,0.0                  ! lower limit for bound check
     :               ,500.0)                 ! upper limit for bound check

       elseif (Variable_name .eq. 'cropsta') then

         call collect_Integer_var (
     :                variable_name        ! variable name
     :               ,'()'              ! units
     :               ,g%cropsta               ! variable
     :               ,numvals              ! number of elements returned
     :               ,0                  ! lower limit for bound check
     :               ,10)                 ! upper limit for bound check


       elseif (Variable_name .eq. 'sbdur') then

         call collect_integer_var (
     :                variable_name        ! variable name
     :               ,'()'              ! units
     :               ,p%sbdur               ! variable
     :               ,numvals              ! number of elements returned
     :               ,0                  ! lower limit for bound check
     :               ,100)                 ! upper limit for bound check

       elseif (Variable_name .eq. 'idoy') then

         call collect_integer_var (
     :                variable_name        ! variable name
     :               ,'()'              ! units
     :               ,g%idoy               ! variable
     :               ,numvals              ! number of elements returned
     :               ,0                  ! lower limit for bound check
     :               ,1000)                 ! upper limit for bound check
!
       elseif (Variable_name .eq. 'dae') then

         call collect_integer_var (
     :                variable_name        ! variable name
     :               ,'()'              ! units
     :               ,g%dae               ! variable
     :               ,numvals              ! number of elements returned
     :               ,0                  ! lower limit for bound check
     :               ,1000)                 ! upper limit for bound check

       elseif (Variable_name .eq. 'estab') then

         call collect_char_var (
     :                variable_name        ! variable name
     :               ,'()'              ! units
     :               ,p%estab               ! variable
     :               ,numvals)              ! number of elements returned

!   dsg 160609  incorporated for rat-grazing by Peter Brown
       elseif (Variable_name .eq. 'rat_graze_perc') then

         call collect_real_var (
     :                variable_name        ! variable name
     :               ,'()'                 ! units
     :               ,g%rat_graze_perc     ! variable
     :               ,numvals              ! number of elements returned
     :               ,0.0                  ! lower limit for bound check
     :               ,0.3)                 ! upper limit for bound check

      else
         ! Don't know this variable name
         call Message_Unused ()
      endif

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine oryza_prepare ()
* ====================================================================
      implicit none

*+  Purpose
*     <insert here>

*+  Mission Statement
*     Perform preparatory calculations for the next timestep

*+  Changes
*   neilh - 01-09-1995 - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'oryza_prepare')

*- Implementation Section ----------------------------------
      call push_routine (myname)

!-----Set CROPSTA: 0=before sowing; 1=day of sowing; 2=in seedbed;
!                  3=day of transplanting; 4=main growth period
        
!         If(g%cropsta .GE.1)   !g%DAE = g%DAE+1

         IF (g%CROPSTA .EQ. 3) g%CROPSTA = 4
         IF (g%CROPSTA .EQ. 2) THEN
            IF (g%DAE .EQ. p%SBDUR) g%CROPSTA = 3
         END IF
         IF (g%CROPSTA .EQ. 1) THEN
            IF (p%ESTAB.EQ.'transplant') THEN
               g%CROPSTA = 2
             elseIF (p%ESTAB.EQ.'direct-seed') THEN
               g%CROPSTA = 4
            END IF
         END IF

!-----Calculate potential soil evaporation and transpiration
        CALL Oryza_ET() !same arguments
      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine oryza_process ()
* ====================================================================
      implicit none


*+  Purpose
*     <insert here>

*+  Mission Statement
*     Perform actions for current day

*+  Changes
*     21-01-1997 - n.huth - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'oryza_process')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (g%plant_status .ne. status_out) then
        call oryza_process1 ()
        call oryzamodel ()
        call oryza_process3 ()
      endif

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine oryza_process1 ()
* ====================================================================
      implicit none

*+  Purpose
*     <insert here>

*+  Mission Statement
*     Day counters after emergence

*+  Changes
*   neilh - 01-09-1995 - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'oryza_process1')

*+  Local Variables
      integer num_layers
      integer i

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine oryza_process3 ()
* ====================================================================
      implicit none

*+  Purpose
*     Determine crop maturity

*+  Mission Statement
*     Determine crop maturity

*+  Changes
*   neilh - 01-09-1995 - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'oryza_process3')

*+  Local Variables
      integer num_layers
      integer i

*- Implementation Section ----------------------------------
      call push_routine (myname)

      !! Keep a track of the max lai for cover_tot calculation
      if (g%rlai.gt.g%max_rlai) then
          g%max_rlai = g%rlai
      endif

      !!== Termination when there is too little N in leaves
      IF (g%RLAI .GT. 1. .AND. g%FNLV .LE. 0.5*g%NMINL) THEN
         call warning_error (ERR_USER,
     :               'Leaf N < 0.5*MINIMUM; crop death')
         !! XXX should call crop_death()??
         g%plant_status = status_dead
      END IF

      !!== Termination when too cold
      IF (g%NCOLD.GT.3.0) then
         call warning_error (ERR_USER,
     :              'Crop death due to 3 days of cold temperatures')
         g%plant_status = status_dead
      ENDIF

      !! Normal maturity
      IF (g%DVS.GT.2.) THEN
         call write_string('Maturity')
         g%plant_status = status_dead
      ENDIF

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine oryza_set_other_variables ()
* ====================================================================
      implicit none

*+  Purpose
*     Update variables owned by other modules.

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'oryza_set_other_variables')
*+  Local Variables
      real    dlt_no3(max_layer)
      real    dlt_nh4(max_layer)
      real    nfract(max_layer)
      real    totno3
      real    totnh4
      real    from_nh4_pool   ! the component of total plant uptake to be taken from the nh4 pool
      integer num_layers, numvals
      integer layer
      integer deepest_layer


*- Implementation Section ----------------------------------
      call push_routine(myname)
      num_layers = count_of_real_vals(p%tkl,max_layer)
      num_layers = max(1,num_layers)

      if (p%nitroenv .eq. env_limited) then
         !! divvy up g%NACR over profile, proportional to N content.
         !  dsg 280408 divvy between no3 & nh4 pools (use NO3 as first preference)
         deepest_layer = find_layer_no (g%zrt * 1000.0
     :                                  ,p%tkl
     :                                  ,max_layer)
         totno3 = sum_real_array(g%no3, deepest_layer)
         totnh4 = sum_real_array(g%nh4, deepest_layer)

! dsg 280408 try to get all g%nacr out of the NO3 if possible
!            - if not, use all NO3, then get remainder of g%nacr from NH4

         if (totno3.ge.g%nacr) then
             ! all of plant N uptake available from NO3 pool, dlt_nh4 = 0.0
             do layer = 1, deepest_layer
              nfract(layer) = divide(g%no3(layer),totno3,0.0)
              dlt_no3(layer) = min(g%no3(layer), g%nacr * nfract(layer))
             enddo
             call set_real_array (unknown_module, 'dlt_no3', '(kg/ha)'
     :                            ,-1.0*dlt_no3, deepest_layer)

         else
            ! use all NO3 pool for plant N uptake, and get remainder from NH4 pool in proportion
            from_nh4_pool = g%nacr - totno3
             do layer = 1, deepest_layer

              nfract(layer) = divide(g%nh4(layer),totnh4,0.0)
              dlt_nh4(layer) = min(g%nh4(layer), ! dsg 280408 get rest from nh4
     :                          from_nh4_pool * nfract(layer))

             enddo

             call set_real_array (unknown_module, 'no3', '(kg/ha)'
     :                            ,0.0*dlt_no3, deepest_layer)

             call set_real_array (unknown_module, 'dlt_nh4', '(kg/ha)'
     :                            ,-1.0*dlt_nh4, deepest_layer)



         endif
      endif

      if (p%prodenv .eq. env_limited .and. 
     :    p%uptake_source .eq. 'calc') then
         call set_real_array (unknown_module
     :                    , 'dlt_sw_dep', '(mm)'
     :                    , -1.0*g%trwl(:), num_layers)
      endif

      call pop_routine(myname)
      return
      end subroutine


*     ===========================================================
      subroutine oryza_ONNew_Profile (variant)
*     ===========================================================
      implicit none

*+  Purpose
*     Update internal soil layer structure with new data

*+  Mission Statement
*     Update internal soil layer structure with new data

*+  Changes
      integer variant

*+  Local Variables
      integer    numvals
      integer    layer
      type(NewProfileType) :: newProfile
!
!*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'oryza_ONNew_Profile')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      newProfile%dlayer(:) = 0
      newProfile%air_dry_dep(:) = 0
      newProfile%ll15_dep(:) = 0
      newProfile%dul_dep(:) = 0
      newProfile%sat_dep(:) = 0
      newProfile%sw_dep(:) = 0
      newProfile%bd(:) = 0

      call unpack_newProfile(variant, newProfile)

      p%tkl = newProfile%dlayer
      p%wcad = newProfile%air_dry_dep
      p%wcwp = newProfile%ll15_dep
      p%wcfc = newProfile%dul_dep
      p%wcst = newProfile%sat_dep

      g%TKLT = 0.0
      ! cvt to (mm3/mm3)
      numvals = count_of_real_vals (p%tkl, max_layer)
      do 1000 layer = 1, numvals
         p%wcad(layer) = divide (p%wcad(layer)
     :                           , p%tkl(layer), 0.0)
         p%wcwp(layer) = divide (p%wcwp(layer)
     :                           , p%tkl(layer), 0.0)
         p%wcfc(layer) = divide (p%wcfc(layer)
     :                           , p%tkl(layer), 0.0)
         p%wcst(layer) = divide (p%wcst(layer)
     :                           , p%tkl(layer), 0.0)
         g%TKLT = g%TKLT + p%tkl(layer)
1000  continue

      call pop_routine (myname)
      return
      end subroutine


*     ===========================================================
      subroutine oryza_OnNewMet (variant)
*     ===========================================================

      implicit none

      integer, intent(in) :: variant

*+  Purpose
*       Obtain all relevant met data

*+  Mission Statement
*       Obtain all relevant met data

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'oryza_OnNewMet')

*+  Local Variables
      type(newmetType) :: newmet
      real vpd

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call unpack_newmet(variant, newmet)

      vpd = oryza_vpd(newmet%vp, newmet%mint, newmet%maxt)

      g%fvpd = linear_interp_real(vpd,p%vpd,p%Fvpd,p%numVPD)

      g%tmmx = newmet%maxt
      g%tmmn = newmet%mint
      g%DTR  = newmet%radn*1000000.0 !convert from MJ/m2/day to J/m2/day
      g%rain = newmet%rain
      g%TMDA = (g%tmmx+g%tmmn)/2.0

      call pop_routine (myname)
      return
      end subroutine

*====================================================================
      real function oryza_VPD (
     :                          vp
     :                        , mint
     :                        , maxt)
*====================================================================

      implicit none


*+  Sub-Program Arguments
      real       vp             ! (INPUT) vapour pressure (hPa = mbar)
      real       mint           ! (INPUT) minimum temperature (oC)
      real       maxt           ! (INPUT) maximum temperature (oC)

*+  Purpose
*     calculate the vapour pressure deficit

*+  Changes
*

*+  Calls

*+  Local Variables
      real       VPDmint !VPD at minimium temperature
      real       VPDmaxt !VPD at maximium temperature

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'oryza_VPD')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      VPDmint = oryza_svp(mint) - vp
      VPDmint = l_bound(VPDmint,0.0)
      VPDmaxt = oryza_svp(maxt) - vp
      VPDmaxt = l_bound(VPDmaxt,0.0)

      oryza_VPD = svp_fract * VPDmaxt
     :             + (1.0 - svp_fract) * VPDmint

      call pop_routine (myname)
      return
      end function

!*     ===========================================================
      subroutine oryza_ONtick (variant)
!*     ===========================================================
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
      integer junk

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'oryza_ONtick')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call unpack_time(variant, tick)
      call jday_to_day_of_year(dble(tick%startday), g%idoy, junk)

      call pop_routine (myname)
      return
      end subroutine



*====================================================================
      real function oryza_svp (temperature)
*====================================================================

      implicit none


*+  Sub-Program Arguments
      real       temperature           ! (INPUT) temperature (oC)

*+  Purpose
*     calculate the saturated vapour pressure for a given temperature

*+  Changes
*       291098 - NIH adapted from Eo module

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'oryza_svp')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      oryza_svp = svp_A*exp(svp_B*temperature/(temperature + svp_C))

      call pop_routine (myname)
      return
      end function

      end module oryzaModule

!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use oryzaModule
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
         allocate(id)
         ! Zero these fields now, as they are used in on_newmet, which
         ! may be received before we do our other initialisations
         p%vpd(:) = 0.0
         p%fvpd(:) = 0.0
         p%numvpd = 0
      else
         deallocate(g)
         deallocate(p)
         deallocate(id)
      end if
      return
      end subroutine



* ====================================================================
       subroutine Main (Action, Data_string)
* ====================================================================
      use oryzaModule
      implicit none
      ml_external Main


*+  Sub-Program Arguments
       character Action*(*)            ! Message action to perform
       character Data_string*(*)       ! Message data

*+  Purpose
*      This routine is the interface between the main system and the
*      oryza module.

*+  Mission Statement
*     APSIM oryza Module

*+  Changes
*     190599 jngh removed reference to version and mes_presence
*     191099 dph  moved the zero_variables to the action_create handler

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'apsim_oryza')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (Action.eq.ACTION_Create) then

      elseif (Action.eq.ACTION_Init) then
         ! Initialise & read constants, parameters
         call oryza_create ()
         call write_string ('   - Initialising')
         call oryza_Init ()

      else if (Action.eq.ACTION_Prepare) then
         call oryza_prepare ()

      else if (Action.eq.ACTION_Process) then
         call oryza_get_other_variables () ! by Xike, 6/11/03
         call oryza_process ()
         call oryza_set_other_variables ()

      elseif (action.eq.ACTION_end_crop) then
         call oryza_end_crop ()

      else if (Action.eq.ACTION_Get_variable) then
         call oryza_Send_my_variable (Data_string)

      else if (Action.eq.ACTION_Set_variable) then
         call oryza_Set_my_variable (data_string)

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
      use OryzaModule

      ml_external doInit1
!STDCALL(doInit1)

      ! set up system interfaces
      call doRegistrations(id)

      end subroutine

! ====================================================================
! This routine is the event handler for all events
! ====================================================================
      subroutine respondToEvent(fromID, eventID, variant)
      use oryzaModule
      implicit none
      ml_external respondToEvent
!STDCALL(respondToEvent)

      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant

      if (eventID .eq. id%tick) then
         call oryza_ONtick(variant)
      elseif (eventID .eq. id%newmet) then
         call oryza_ONnewmet(variant)
      elseif (eventID .eq. id%new_profile) then
         call oryza_ONNew_Profile(variant)
      elseif (eventID .eq. id%sow) then
         call Oryza_start_crop (variant)
      else
         ! Nothing I know about??
      endif

      return
      end subroutine respondToEvent

