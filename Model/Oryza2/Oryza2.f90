! 23/12/2011 Things to do:
! FBiomassRemovedType (incorp surfaceOM at end_crop) is broken
! no root weight (by layer) is appearing -> root transfer to FOM pool isnt happening
! no canopy radiation partitioning
! no interaction with swim
! no rat grazing
! variable naming probing needs help (ie sow a crop for getDescription)
! latest & greatest .ini file
! sand/clay is gotten twice

      module Oryza2Module
      use Infrastructure2

      integer    max_layer
      parameter (max_layer = 20)

      type Oryza2Globals
        sequence
        logical hasInitialised

!----- CROPSTA: 0=before sowing; 1=day of sowing; 2=in seedbed;
!                  3=day of transplanting; 4=main growth period

        integer cropsta
        integer iday, iyear
        REAL    LAT  
        type(NewProfileType) :: SoilProfile
        type(NewMetType)     :: met
        real met_vp
        real met_windspeed
            
        character(100) crop_type
        character(100) sow_cultivar 
        character(100) sow_establishment 
        character(100) etmod
        real sow_sbdur
        real sow_nplh
        real sow_nh
        real sow_nplsb
        real sow_nplds
        logical sow_today
        integer dae, dat 
        character*8   plant_status
        real max_rlai
        real cover_green
        real cover_tot
        real          eo                       ! EO from system (mm/d)
        real          pond_depth
        real anga, angb
        logical Soil_has_N
        logical Soil_has_sw
        real no3(max_layer)       ! amount of NO3 in each soil layer (kg/ha)
        real nh4(max_layer)       ! amount of NH4 in each soil layer (kg/ha)
        real ureappm(max_layer)   ! amount of Urea in each soil layer (ppm)
        real SomLitC(0:max_layer)      !Soil organic C (kg/ha) (unused)
        real SomLitE(0:max_layer,5042) !Soil organic N (kg/ha) (unused)
        real st(max_layer)        ! Soil temperature (oC)
        real sw(max_layer)        ! Soil water content (mm/mm)
        real Soil_sand(max_layer)
        real Soil_clay(max_layer)
        real soil_kl(max_layer)
        !real Soil_alphaVg(max_layer)
        !real Soil_mVg(max_layer)
        !real Soil_nVg(max_layer)
        real RNSTRS, LDSTRS, tnsoil
        REAL height                     ! Canopy height (mm)
        REAL dlt_surfaceom              ! Surface residues (kg/ha)
        REAL dlt_surfaceom_n            ! Surface residues N (kg/ha)
        REAL dlt_root_mass(max_layer)   ! Root residues (kg/ha)
        REAL dlt_root_mass_n(max_layer) ! Root residues N (kg/ha)
        real dlt_sw_dep(max_layer)      ! soil water uptake (mm)
        real dlt_no3(max_layer)         ! no3 uptake (mm)
        real dlt_nh4(max_layer)         ! nh4 uptake (mm)
        real RLV(max_layer)             ! root length density (mm)
        real ZRTMS                      ! Max root depth
        real harvestFraction            ! fraction of biomass removed when harvesting
        end type Oryza2Globals

      common /instancePointers/ ID, g, p, c
      save /InstancePointers/
      type (Oryza2Globals),pointer :: g
      integer,pointer :: ID, p, c

      contains
!     ===========================================================
      subroutine oryza2_zero_variables ()
!     ===========================================================
      implicit none
!+  Purpose
!     Set all state variables in this module to zero.

!+  Mission Statement
!     Initialise module state variables

!+  Changes
!     <insert here>

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'oryza2_zero_variables')

!- Implementation Section ----------------------------------
      g%hasInitialised = .false.
      g%iday = 0
      g%iyear = 0
      g%dat = 0
      g%dae = 0
      g%eo = 0
      g%lat = -999.0
      g%pond_depth = 0.0
      g%no3(:) = 0.0
      g%nh4(:) = 0.0
      g%SomLitC(:) = 0.0
      g%SomLitE(:,:) = 0.0
      g%st(:) = 0.0
      g%sw(:) = 0.0
      g%ureappm(:) = 0.0
 
      g%max_rlai = 0.0
      g%height = 500.0
      g%cover_green = 0.0
      g%cover_tot = 0.0

      g%dlt_surfaceom = 0.0      
      g%dlt_surfaceom_n = 0.0      
      g%dlt_root_mass(:) = 0.0      
      g%dlt_root_mass_n(:) = 0.0
      g%dlt_sw_dep(:) = 0.0
      g%dlt_no3(:) = 0.0
      g%dlt_nh4(:) = 0.0
      g%rlv(:) = 0.0
      g%sow_cultivar = ' '
      g%sow_establishment = ' '
      g%sow_sbdur = 0.0
      g%sow_nplh = 0.0
      g%sow_nh =  0.0
      g%sow_nplsb = 0.0
      g%sow_today = .false.
      g%SoilProfile%num_dlayer = 0
      g%Soil_sand(:) = 0.0
      g%Soil_clay(:) = 0.0
      g%Soil_kl(:) = 0.0
      !g%Soil_alphaVg(:)= 0.0
      !g%Soil_mVg(:)= 0.0
      !g%Soil_nVg(:)= 0.0
      g%Soil_has_n= .false.
      g%Soil_has_sw= .false.
      g%etmod = ' '
        g%anga= 0.0
        g%angb= 0.0
        g%tnsoil = 0.0
        g%RNSTRS = 1.0
        g%LDSTRS = 1.0
      g%zrtms = 0.0
      g%harvestFraction = 0.0
      g%plant_status = 'out'
      g%cropsta = 0
      return
      end subroutine

!     ===========================================================
      subroutine oryza2_get_other_variables ()
!     ===========================================================
      use ScienceAPI2
      implicit none

!+  Purpose
!      Get the values of variables from other modules
      logical found
      integer numvals
!- Implementation Section ----------------------------------

      if (g%etmod .eq. 'PENMAN') then
        ! inputs required for internal PM.
        found = get('vp', '(mm)', 0, g%met_vp, 0.0, 10000.0)
        found = get('windspeed', '()', 0, g%met_windspeed, 0.0, 10000.0)
      else if (g%etmod .eq. 'MAKKINK' .or. g%etmod .eq. 'PRIESTLEY TAYLOR') then
        ! nothing to do
      else 
        found = get(g%etmod, '(mm)',  0, g%eo, 0.0, 100.0)
      endif

      found = get('pond_depth', '(mm)', 1, g%pond_depth, 0.0, 10000.0)
      if (.not. found) g%pond_depth = 0.0

      g%soil_has_n = .true.
      found = get ('no3',     '(kg/ha)' ,   1, g%no3,  numvals, max_layer, 0.0, 10000.0)
      if (.not. found) then
        g%soil_has_n = .false.
      else
        found = get ('nh4',     '(kg/ha)' ,   0, g%nh4,      numvals, max_layer, 0.0, 10000.0)
        found = get ('ureappm', '(kg/ha)' ,   0, g%ureappm,  numvals, max_layer, 0.0, 100000.0)
        found = get ('st',      '(oC)' ,      0, g%st,       numvals, max_layer, 0.0, 50.0)
      end if

      g%soil_has_sw = get ('sw',      '(mm/mm)'    , 1, g%sw,  numvals, max_layer, 0.0, 1.0)

      return
      end subroutine

!     ===========================================================
      subroutine oryza2_set_other_variables ()
!     ===========================================================
      use ScienceAPI2
      implicit none
!+  Purpose
!      Set the values of variables in other APSIM modules
      type(FOMLayerType) :: IncorpFOM
      type(BiomassRemovedType) :: BiomassRemoved
      type(CropChoppedType) :: CropChopped
      integer layer
	  character*(100)  message

!- Implementation Section ----------------------------------
      if (g%Soil_has_N .and. sum(g%dlt_no3) .ne. 0.0) then
        call SetRealArray ('dlt_no3', '(kg/ha)' &
                           ,-1.0*g%dlt_no3, g%SoilProfile%num_dlayer)
      endif

      if (g%Soil_has_N .and. sum(g%dlt_nh4) .ne. 0.0) then
        call SetRealArray ('dlt_nh4', '(kg/ha)' &
                           ,-1.0*g%dlt_nh4, g%SoilProfile%num_dlayer)
      endif

      if (g%Soil_has_sw .and. sum(g%dlt_sw_dep) .ne. 0.0) then
        call SetRealArray ('dlt_sw_dep', '(mm)' &
                         ,-1.0*g%dlt_sw_dep, g%SoilProfile%num_dlayer)
      endif

      if (sum(g%dlt_root_mass) .ne. 0.0) then
         write (message,*) 'adding ', sum(g%dlt_root_mass) , 'kg roots'
		 call writeLine(message)
         IncorpFOM%Type = g%crop_type
         IncorpFOM%num_layer = g%SoilProfile%num_dlayer
         do layer = 1, g%SoilProfile%num_dlayer
           IncorpFOM%layer(layer)%LabileP = 0.0
           IncorpFOM%layer(layer)%CNR = 0.0
           IncorpFOM%layer(layer)%FOM%Amount = g%dlt_root_mass(layer)
           IncorpFOM%layer(layer)%FOM%N = g%dlt_root_mass_n(layer)
           IncorpFOM%layer(layer)%FOM%C = 0.4 * g%dlt_root_mass(layer)
           IncorpFOM%layer(layer)%FOM%P = 0.0
           IncorpFOM%layer(layer)%FOM%AshAlk = 0.0
         end do
         call PublishFOMLayerType('incorp_fom', IncorpFOM) 
      endif

      if (g%dlt_surfaceom .gt. 0.1) then
          write (message,'(a, f7.1, a)') 'Adding ', g%dlt_surfaceom , ' kg/ha to surface residues'
 		  call writeLine(message)
          BiomassRemoved%dm_type(:) = ' '
          BiomassRemoved%dlt_crop_dm(:)         = 0.0
          BiomassRemoved%dlt_dm_n(:)            = 0.0
          BiomassRemoved%dlt_dm_p(:)            = 0.0 
          BiomassRemoved%fraction_to_residue(:) = 0.0

          BiomassRemoved%crop_type = g%crop_type
          BiomassRemoved%dm_type(1)             = g%crop_type
          BiomassRemoved%num_dm_type            = 1
          BiomassRemoved%dlt_crop_dm(1)         = g%dlt_surfaceom
          BiomassRemoved%num_dlt_crop_dm        = 1
          BiomassRemoved%dlt_dm_n(1)            = g%dlt_surfaceom_n
          BiomassRemoved%num_dlt_dm_n           = 1
          BiomassRemoved%num_dlt_dm_p           = 0
          BiomassRemoved%fraction_to_residue(1) = 1.0
          BiomassRemoved%num_fraction_to_residue= 1
          !call PublishBiomassRemovedType('BiomassRemoved', BiomassRemoved) !FIXME causes infrastructure crash
		  !CropChopped%crop_type = g%crop_type
		  !CropChopped%dm_type(1) = g%crop_type
		  !CropChopped%num_dm_type = 1
		  !CropChopped%dlt_crop_dm(1) = g%dlt_surfaceom
		  !CropChopped%num_dlt_crop_dm = 1
		  !CropChopped%dlt_dm_n(1) = g%dlt_surfaceom_n
		  !CropChopped%num_dlt_dm_n = 1
		  !CropChopped%fraction_to_Residue(1) = 1.0
		  !CropChopped%num_fraction_to_Residue = 1
          !call PublishCropChoppedType('CropChopped',CropChopped)
      endif
      return
      end subroutine
      end module Oryza2Module

!     ===========================================================
      subroutine CallOryza(controlValue)
!     ===========================================================
      USE ModuleDefs
      USE infrastructure2
      Use Oryza2Module
      use interface_oryza
      USE Public_Module
  	  USE RootGrowth

      implicit none
      ! 1 argument: the control parameter (start/stop/process etc)
      integer controlValue

      ! Local, saved variables
      save

      TYPE (SwitchType)  ISWITCH
      INTEGER         ITASK
      LOGICAL         OR_OUTPUT
      CHARACTER (128) FILEI1, FILEIT, FILEI2, message
      REAL    TIME, DELT  ,  NFLV, NSLLV
      REAL    TKLT  ,  LRSTRS, LESTRS, NRT
      REAL    CPEW , LAIROL, ZRT  , DVS, WCL(10),  DLDR
      REAL    LAI, LLV  , SLA , WLVG  , WST  , WSO, GSO, GGR, GST, GLV, PLTR 
      REAL    TRW, TRWL(10), TKL(10), WRT, WRR14
      REAL    HU, WAGT, WLVD, WRR, NGR, NACR
      REAL    ANSO, ANLV, ANST, ANLD, ANRT
      REAL    FACT, rDOY, rDAE, rdd, etrd, etae, TRC, pcew, TMDA, ETD, EVSC
      REAL, DIMENSION(10) :: MSKPA, MSUC
      REAL, DIMENSION(2)  :: HARVFRAC
      REAL, DIMENSION(NL) :: UPPM
      INTEGER NLAYR

      INTEGER YEAR, IUNITD, IUNITL, L
      LOGICAL TERMNL

      if (controlValue .ne. SEASINIT .and. .not. g%hasInitialised) return ! Do nothing until initialisation
      
!***********************************************************************
!     Seasonal initialization - run once per planting season
!***********************************************************************
      IF (controlValue == SEASINIT) THEN
        call oryza2_get_other_variables ()

      ! Set switches
      OR_OUTPUT = .true.

      if (g%pond_depth .GT. 0) then
        ISWITCH%IIRRI = 'Y'
      else
        ISWITCH%IIRRI = 'N'
      endif

      if (g%Soil_has_sw) then
        ISWITCH%ISWWAT = 'Y'
        NLAYR  =  g%SoilProfile%num_dlayer
      else 
        ISWITCH%ISWWAT = 'N'
        ! Generate some dummy values that aren't read..
        NLAYR  =  10
        g%SoilProfile%dlayer(:) = 100.0
      endif

      if (g%Soil_has_N) then
        ISWITCH%ISWNIT = 'Y'
      else
        ISWITCH%ISWNIT = 'N'
      endif

        ITASK = 1
        TIME = 0.0
        TERMNL = .FALSE.
        ALLOCATE(pv)                              !Added by TaoLi, 24 April 2011
        Call SET_ZERO_PV

        IF (NLAYR > 10) THEN
          call fatal_error(err_user, "Too many layers for ORYZA2000 model.")
        ENDIF

        HU = 0.0
        WAGT = 0.0
        WLVD = 0.0
        WLVG = 0.0
        WST = 0.0
        WSO = 0.0
        WRT = 0.0
        WRR14 = 0.0
        NGR = 0.0
        GSO = 0.0
        GGR = 0.0
        GST = 0.0
        GLV = 0.0
        PLTR = 0.0
        LLV = 0.0
        WRR = 0.0
        SLA = 0.0
        LAI = 0.0
        LAIROL = 0.0
        ZRT = 0.0
        DVS = 0.0
        DLDR =0.0
        ANSO = 0.0
        ANLV = 0.0
        ANST = 0.0
        ANLD = 0.0
        ANRT = 0.0
            TMDA = 0.0
        EVSC = 0.0
            ETD = 0.0
!       Water & N stresses
        LRSTRS = 1.0
        g%LDSTRS = 1.0
        LESTRS = 1.0
        PCEW   = 1.0
        CPEW   = 1.0
        g%RNSTRS = 1.0

        PV % PNL = NLAYR
        IF(INDEX(ISWITCH%ISWWAT,"Y").GT.0) THEN
         TKLT = 0.0
         DO L=1, NLAYR
          pv%PResC(L,1) = 0.0          !       Cumulative senesced N to surface and soil
          PV%PResN(L,1) = 0.0
          PV%PBD(L)     = g%SoilProfile%bd(L)    
          PV%PDLAYER(L) = g%SoilProfile%dlayer(L)
          TKL(L) = g%SoilProfile%dlayer(L) / 1000.0                            ! Convert to m
          TKLT = TKLT + TKL(L)
          if (g%zrtms .le. 0.0) then
            ! Only use fractional values in lowest layer
            if (g%Soil_kl(L) < 1.0 .and. L .ne. NLAYR) then
              g%zrtms = g%zrtms + TKL(L)
            else
              g%zrtms = g%zrtms + TKL(L) * g%Soil_kl(L)
            endif
          endif
          PV%PWCST(L) = g%SoilProfile%SAT_dep(L) / g%SoilProfile%dlayer(L) 
          PV%PWCFC(L) = g%SoilProfile%DUL_dep(L) / g%SoilProfile%dlayer(L) 
          PV%PWCWP(L) = g%SoilProfile%LL15_dep(L) / g%SoilProfile%dlayer(L)
          PV%PWCAD(L) = g%SoilProfile%air_dry_dep(L) / g%SoilProfile%dlayer(L)
          PV%Psand(L) = g%Soil_sand(L)  ! These are redundant - read again in Oryza1.f90:376
          PV%Pclay(L) = g%Soil_clay(L)
         end do
        ELSE
          pv%PResC(:,:) = 0.0
          PV%PResN(:,:) = 0.0
          PV%PBD(:)   = 1.2
          PV%PDLAYER(:) = 100
          TKL(:) = 0.1
          PV%PWCST(:) = 0.5
          PV%PWCFC(:) = 0.4
          PV%PWCWP(:) = 0.3
          PV%PWCAD(:) = 0.2
          PV%Psand(:) = 0.5
          PV%Pclay(:) = 0.5
          g%zrtms = sum(tkl) 
        ENDIF
!       ESSENTIAL INFORMATION MUST BE PROVIDED FROM UPPER LAYER
        FILEI1 = 'nul'  ! FIXME should use /dev/null on unix
        FILEIT = 'nul'
        FILEI2 = 'nul'

        TERMNL = .FALSE.
        IF ((INDEX(ISWITCH%ISWNIT,"Y").gt. 0) .OR. (INDEX(ISWITCH%ISWWAT,"Y") .gt. 0)) THEN
            PV%PROOT_NUTRIENT = .TRUE.
        ELSE
            PV%PROOT_NUTRIENT = .FALSE.; FILEI2=""
        END IF

        CALL GETLUN("ORYZA1",IUNITD)
        CALL GETLUN("ORYZA2",IUNITL)
        IUNITD = IUNITD+10
        IUNITL = IUNITL+20
        
        DELT = 1.0  !TIME STEP IS 1.0`
        g%DAE = 0.0        
        
!***********************************************************************
!***********************************************************************
!     Rate - daily
!***********************************************************************
      ELSEIF (controlValue == RATE) THEN
        ITASK = 2
        ! Transfer APSIM variables into ORYZA variables
        call oryza2_get_other_variables ()
        IF(INDEX(ISWITCH%ISWWAT,"Y").GT.0) THEN

         DO L = 1, NLAYR
          WCL(L) = g%SoilProfile%sw_dep(L) / g%SoilProfile%dlayer(L) !Soil water content (mm3/mm3)
          pv%Pswc(L)   = WCL(L)                                      !Soil water content (mm3/mm3)
          pv%pSOILTx(L) = g%st(L)      ! Soil temperature (oC)
          PV % PNO3(L) = g%NO3(L)      ! NO3 (kg/ha)
          PV % PNH4(L) = g%NH4(L)      ! NH4 (kg/ha)

!------- Calculate moisture suction in KPa
! Code from paddy.f90 (standalone)
!--------------Calculate moisture suction MSUC(I) in cm H2O
          IF (WCL(L).GE.pv%PWCFC(L)) THEN
             FACT = (pv%PWCST(L)-WCL(L))/(pv%PWCST(L)-pv%PWCFC(L))
             FACT = MAX(0., MIN(1.,FACT))
             MSUC(L) = 10.**(FACT*2.0)
             IF (WCL(L).GE.pv%PWCST(L)) MSUC(L) = 0.
          ELSE IF (WCL(L).GE.pv%PWCWP(L).AND.WCL(L).LT.pv%PWCFC(L)) THEN 
             FACT = (WCL(L)-pv%PWCWP(L))/(pv%PWCFC(L)-pv%PWCWP(L))
             FACT = MAX(0., MIN(1.,FACT))
             MSUC(L) = 10.**(4.2-FACT*2.2)
          ELSE IF (WCL(L).LT.pv%PWCWP(L)) THEN
             FACT = (WCL(L)-pv%PWCAD(L))/(pv%PWCWP(L)-pv%PWCAD(L))
             FACT    = MAX(0., MIN(1.,FACT))
             MSUC(L) = 10.**(7.0-FACT*2.8)
          END IF
!      Note: MSKPA(I) is matrix moisture suction in kPa!
          MSKPA(L) = (MSUC(L)/10.)
         ENDDO
        ELSE 
          WCL(:) = 0.3
        ENDIF

        g%dlt_nh4(:) = 0.0
        g%dlt_no3(:) = 0.0
        g%dlt_sw_dep(:) = 0.0
        TMDA = (g%met%mint + g%met%maxt) / 2.0
        rdd = g%met%radn * 1000000.0

!-----  Set CROPSTA: 0=before sowing; 1=day of sowing; 2=in seedbed;
!                  3=day of transplanting; 4=main growth period
        IF (g%CROPSTA .EQ. 0 .and. g%sow_today) THEN
           g%CROPSTA = 1
        ELSE IF (g%CROPSTA .EQ. 1) then
            if(g%sow_establishment .eq. 'DIRECT-SEED' ) THEN
                g%CROPSTA = 4
            else
                g%CROPSTA = 2
            end if
            call WriteLine(' Sowing in seedbed')
        ELSE IF (g%CROPSTA .EQ. 2  .and. g%dae .ge. g%sow_sbdur) THEN
           call WriteLine('Day of transplanting')
           g%CROPSTA = 3
        ELSE IF (g%CROPSTA .EQ. 3) then
           call WriteLine(' Entering main growth period')
           g%CROPSTA = 4
        END IF
                              
!***********************************************************************
!***********************************************************************
!     Integration - daily
!***********************************************************************
     ELSEIF (controlValue == INTEGR) THEN

        ITASK = 3
        TIME = TIME+DELT

!***********************************************************************
!***********************************************************************
!     Daily or seasonal output
!***********************************************************************
      ELSE
        ITASK = 0
        IF (controlValue == SEASEND) THEN
          TERMNL = .TRUE.
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
    IF(.NOT.TERMNL ) THEN
        if (g%ETMOD .ne. ' ') then
           call ET2(ITASK, OR_OUTPUT, g%ANGA , g%ANGB   , RDD , TMDA, g%met_vp  , g%met_windspeed , g%lat, &
                    g%iday , 1.0, g%ETMOD, g%CROPSTA, g%sow_establishment, NL  , 1.0 , g%pond_depth,    &
                    WCL, pv%PWCST , LAI, EVSC, ETD , TRC)
        else
           trc = g%eo
        endif

      IF(INDEX(ISWITCH%ISWWAT,"Y").GT.0) THEN

            CALL WSTRESS2 (ITASK,  DELT,   OR_OUTPUT, IUNITD, IUNITL, FILEI1, FILEIT, &
                          TRC,    ZRT,    TKL,    NLAYR,    g%CROPSTA, &
                          WCL,    pv%PWCWP,   MSKPA, &
                          TRW,    TRWL,   LRSTRS, g%LDSTRS, LESTRS, PCEW, CPEW)
        !       Check for potential production condition  
        ELSEIF(INDEX(ISWITCH%ISWWAT, "N").GT.0) THEN              !POTENTIAL WATER CONDITION
            TRW = TRC; TKLT = SUM(TKL); g%ZRTMS = TKLT   !THE TOTAL TRANSPIRATION EQUALS TO POTENTIAL TRANSPIRATION
            CALL WNOSTRESS (NLAYR, TRW, TRWL, ZRT, TKL, LRSTRS, g%LDSTRS, LESTRS, PCEW, CPEW)
        END IF

        rDOY = REAL(g%iday)
        rDAE = REAL(g%DAE)
        CALL ORYZA1(ITASK,  IUNITD, IUNITL, FILEI1, FILEI2,FILEIT, &
                        OR_OUTPUT, TERMNL, g%iday  , rDOY, &
                        TIME,   DELT,   g%LAT,    rdd,    g%met%mint,   g%met%maxt, &
                        NFLV,   NSLLV,  NRT,      g%RNSTRS,                 &
                        g%sow_Establishment,  TKLT,   g%ZRTMS,  g%CROPSTA, &
                        LRSTRS, g%LDSTRS, LESTRS, PCEW,  CPEW, TRC, &
                        rDAE,    SLA, LAI,    LAIROL, ZRT,    DVS, &
                        LLV,    DLDR, WLVG, WST, WSO, GSO, GGR, GST, GLV, &
                        PLTR, WCL,  g%pond_depth, WRT, WRR14, NGR, HU)

        IF(INDEX(ISWITCH%ISWNIT, "N").GT.0) THEN           !POTENTIAL NITROGEN CONDITION
            CALL NNOSTRESS2(DELT, IUNITD, IUNITL, ITASK, FILEI1, FILEIT, &
                           g%CROPSTA, DVS, WLVG, LAI, SLA, NFLV, NSLLV, g%RNSTRS)
        ELSE
            CALL NCROP3 (ITASK, IUNITD, IUNITL, FILEI1, FILEI2, FILEIT, DELT, TIME, OR_OUTPUT, &
                        TERMNL, DVS, LLV, DLDR, WLVG, WST, WSO, GSO, GST, GLV, &
                        PLTR, LAI, SLA, g%CROPSTA, g%TNSOIL, NACR, ANSO, ANLV, ANST, ANLD, &
                        ANRT, NFLV, NSLLV,NRT, g%RNSTRS)
        END IF

      IF (ITASK == 2) THEN
        DO L = 1, NLAYR
  	      g%dlt_no3(L) =  (g%no3(L) - PV % PNO3(L))    !NO3 uptake (kg/ha)
          g%dlt_nh4(L) =  (g%nh4(L) - PV % PNH4(L))    !NH4 uptake (kg/ha)
          g%dlt_sw_dep(L) = TRWL(L)                   !H2O uptake (mm/d)
		  
          !         Add senesced roots to soil fresh organic matter pools
          !g%dlt_root_mass(L)  =  pv%PResC(L,1) / 0.40  ! DSSAT has this.
          !g%dlt_root_mass_n(L) = PV%PResN(L,1)
          g%dlt_root_mass(L)  = RRDCL(L) / 0.4  !!! FIXME - nothing happens here!!!
          g%dlt_root_mass_n(L) = RRDNL(L)      
          g%rlv(L) = RRDENSIT(L)               ! pv%prootden(L) !* 1.E-10
        end do
        !write (*,*) 'task = ', itask
        !write (*,*) 'dlt_no3 = ', sum(g%dlt_no3) , 'kg '
        !write (*,*) 'dlt_nh4 = ', sum(g%dlt_nh4) , 'kg '
        !write (*,*) 'dlt_sw = ', sum(g%dlt_sw_dep) , 'mm'
        !write (*,*) 'dlt_root_mass = ', sum(g%dlt_root_mass) , 'kg roots'
      ELSEIF (ITASK == 3) THEN
        WLVD = WLVD+ (DLDR+LLV)*DELT        
        WAGT = WST + WLVG + WSO + WLVD
        WRR  = WRR14 * 0.86

        g%dae= pv%Pdae
        IF (g%CROPSTA .GE. 3) THEN
           g%dat = g%dat + 1
        endif

        !! Keep a track of the max lai for cover_tot calculation
        if (LAI.gt.g%max_rlai) then
            g%max_rlai = LAI
            g%cover_tot = 1 - exp(-0.5 * g%max_rlai)
        endif
        g%cover_green = 1 - exp(-0.5 * LAI)
        call oryza2_set_other_variables()
      ENDIF
    ENDIF

    IF (TERMNL .AND. controlValue == INTEGR) THEN 
	    g%plant_status = 'dead'
    ENDIF
    
    IF (TERMNL .AND. controlValue == SEASEND) THEN 
        g%dlt_surfaceom  = (WLVG + WLVD + WST) * (1.0 - g%harvestFraction)
        g%dlt_surfaceom_n = (ANLV + ANLD + ANST) * (1.0 - g%harvestFraction)
        write (message, '(a, f7.1, a)') &
               'Crop ended. Rough Rice Yield (dw) = ', WRR, ' (kg/ha)'
        call writeLine(message)
        write (message,'(a, f7.1, a)') 'Removing ', (WLVG + WLVD + WST) * g%harvestFraction , ' kg/ha biomass from field'
        call writeLine(message)
        g%dlt_nh4(:) = 0.0
        g%dlt_no3(:) = 0.0
        g%dlt_sw_dep(:) = 0.0
        call oryza2_set_other_variables()
	    g%plant_status = 'out'
        DEALLOCATE(PV) 
    ENDIF
      
      return
      end subroutine


      ! ===========================================================
      ! do first stage initialisation.
      ! ===========================================================
      subroutine OnInit1()
      Use Oryza2Module
      use ScienceAPI2
      use public_module
      implicit none
!STDCALL(OnInit1)
!SDTCALL(Oryza2OnTick,)
!STDCALL(Oryza2OnNewProfile)
!STDCALL(Oryza2OnPrepare)
!STDCALL(Oryza2OnProcess)
!STDCALL(Oryza2OnPost)
!STDCALL(Oryza2OnCommence)
!STDCALL(Oryza2OnNewMet)
!STDCALL(Oryza2OnSow)
!STDCALL(Oryza2OnHarvest)
      external ::Oryza2OnTick, Oryza2OnNewProfile, Oryza2OnPrepare, Oryza2OnProcess, Oryza2OnPost, Oryza2OnCommence, Oryza2OnNewMet, Oryza2OnSow, Oryza2OnHarvest, Oryza2OnEndCrop

      integer found
  
      call oryza2_zero_variables ()
      found = ReadParam('crop_type', '()', 0, g%crop_type)
      call SubscribeTimeType('tick', Oryza2OnTick)
      call SubscribeNewProfileType('new_profile', Oryza2OnNewProfile)
      call SubscribeNullType('prepare', Oryza2OnPrepare)
      call SubscribeNullType('process', Oryza2OnProcess)
      call SubscribeNullType('post', Oryza2OnPost)
      call SubscribeNullType('start_simulation', Oryza2OnCommence)
      call SubscribeNewMetType('newmet', Oryza2OnNewMet)
      call SubscribeSowType('sow', Oryza2OnSow)
      call SubscribeHarvestType('harvest', Oryza2OnHarvest)
      call SubscribeNullType('end_crop', Oryza2OnEndCrop)

      ! Register some non-oryza variables so other apsim modules are happy
      call Expose('plant_status', '', 'Status - in, alive, dead', .false., g%plant_status) 
      call Expose('dae', 'd', 'Days after emergence', .false., g%dae) 
      call Expose('dat', 'd', 'Days after transplanting', .false., g%dat) 
      call Expose('canopy_height', 'mm', 'Canopy Height', .false., g%height)
      call Expose('height', 'mm', 'Canopy Height', .false., g%height)
      call Expose('max_lai', 'm2/m2', 'Rice LAI', .false., g%max_rlai) 
      call Expose('cover_green', '', 'Green cover', .false., g%cover_green) 
      call Expose('cover_tot', '', 'Total (green and dead) cover', .false., g%cover_tot) 
      call Expose('cropsta', '', 'Crop status', .false., g%cropsta) 

      call Expose('tnsoil', '', 'Soil-N available for crop uptake', .false., g%tnsoil) 
      call Expose('rnstrs', '', 'Decrease factor for RGRL caused by N stress', .false., g%rnstrs) 
      call Expose('ldstrs', '', 'Stress factor for leaf death', .false., g%ldstrs) 
      call Expose('rlv', 'mm3/mm3', 'RLV', .false., g%rlv, g%SoilProfile%num_dlayer, max_layer)
      call Expose('dlt_sw_dep', 'mm', 'Water uptake', .false., g%dlt_sw_dep, g%SoilProfile%num_dlayer, max_layer)
      call Expose('dlt_no3_oryza', 'mm', 'NO3 uptake', .false., g%dlt_no3, g%SoilProfile%num_dlayer, max_layer) 
      call Expose('dlt_nh4_oryza', 'mm', 'NH4 uptake', .false., g%dlt_nh4, g%SoilProfile%num_dlayer, max_layer) 

      ! Find what to do about ET
      g%etmod = ' '
      found = ReadParam('kl', '()', 1, g%soil_kl, found, max_layer, 0.0, 1.0)
      found = ReadParam('zrtms', '()', 1, g%zrtms, 0.0, 1000.0)

      end subroutine


!     ===========================================================
      subroutine Oryza2OnCommence()
!     ===========================================================
      Use Oryza2Module
      USE ModuleDefs
      use ScienceAPI2
      implicit none
!STDCALL(Oryza2OnCommence)
      logical found
      integer numvals
      
      ! Find static information from other modules      
      found = Get('latitude', '()', 0, g%lat, -90.0, 90.0)
      found = Get('sand', '()', 0, g%soil_sand, numvals, max_layer, 0.0, 100.0)
      found = Get('clay', '()', 0, g%soil_clay, numvals, max_layer, 0.0, 100.0)

      return
      end subroutine

!     ===========================================================
      subroutine Oryza2OnNewProfile(newProfile)
!     ===========================================================
      Use Oryza2Module
      implicit none
!STDCALL(Oryza2OnNewProfile)
      type(NewProfileType) :: newProfile

      if (newProfile%num_dlayer > 10) then
        call Fatal_error(Err_internal, 'Too many soil layers')
      endif
      g%SoilProfile = newProfile

      return
      end subroutine

!     ===========================================================
      subroutine Oryza2OnNewMet (newmet)
!     ===========================================================
      Use Oryza2Module
      implicit none
!STDCALL(Oryza2OnNewMet)
      type(newmetType) :: newmet
      g%met = newmet

      return
      end subroutine


!     ===========================================================
      subroutine Oryza2OnTick (tick)
!     ===========================================================
      Use Oryza2Module
      implicit none
!STDCALL(Oryza2OnTick)

!+  Purpose
!     Update internal time record.
      type(timeType) :: tick

      call jday_to_day_of_year(tick%startday, g%iday, g%iyear)
      g%sow_today = .false.

      return
      end subroutine

! hmmm. This is a bit of a furphy - oryza starts at emergence.
!     ===========================================================
      subroutine Oryza2OnSow (newSow)
!     ===========================================================
      use ScienceAPI2
      USE ModuleDefs
      Use Oryza2Module
      implicit none
!STDCALL(Oryza2OnSow)
      integer found
      type(sowType) :: newSow
      character*(100)  line
!+  Purpose
!     Start the crop

      if (g%hasInitialised) then
           call fatal_error(err_user, "Crop is already established - call 'end_crop' first.")
           return
	  end if

      call publishNull('sowing')

      ! Some of the strings in a sow structure seem not be initialised correctly, and 
      ! copying the structure gives garbage. So keep a copy of whatever parameters are passed in. 
      g%sow_cultivar      = newSow%CULTIVAR
      g%sow_establishment = newSow%establishment
      call SetSearchOrder(trim(g%sow_Cultivar))
      call upperc(g%sow_establishment)
      IF (g%sow_establishment .NE. 'TRANSPLANT' .AND. g%sow_establishment .NE. 'DIRECT-SEED') THEN
           call fatal_error(err_user, "Unknown establishment method "//g%sow_establishment)
      endif
      call AppendSearchOrder(trim(g%sow_Establishment))

	  call writeLine (' Establishment method = ' // trim(g%sow_establishment))
	  call writeLine (' Cultivar = ' // trim(g%sow_Cultivar))

      if (g%sow_establishment .ne. 'DIRECT-SEED') then
  	    if (newSow%sbdur .gt. 0) then
	      g%sow_sbdur = newSow%sbdur
        else
          found = ReadParam('sbdur', '()', 1, g%sow_sbdur, 0.0, 60.0)
	    end if 	
		write (line, *) ' Duration in seedbed = ' , g%sow_sbdur
	    call writeLine (line)
	  else 
        g%sow_sbdur = 0
      end if
	  
      if (g%sow_establishment .eq. 'TRANSPLANT') then
        if (newSow%nplh .gt. 0) then
	      g%sow_nplh = newSow%nplh
        else	  
          found = ReadParam('nplh', '()', 0, g%sow_nplh, 0.0, 500.0)
	    end if
	    write (line, *) ' Plants / hill = ' , g%sow_nplh
        call writeLine (line)
	    
        if (newSow%nh .gt. 0) then
	      g%sow_nh = newSow%nh
        else	  
          found = ReadParam('nh', '()', 0, g%sow_nh, 0.0, 500.0)
	    end if
	    write (line, *) ' Hills / m^2 = ' , g%sow_nh
        call writeLine (line)

        if (newSow%nplsb .gt. 0) then
	      g%sow_nplsb = newSow%nplsb
        else	  
          found = ReadParam('nplsb', '()', 0, g%sow_nplsb, 0.0, 500.0)
	    end if
	    write (line, *) ' Number of plants in seedbed (pl/m^2) = ' , g%sow_nplsb
        call writeLine (line)
      else
	    g%sow_nplh = 0
		g%sow_nh = 0
        g%sow_nplsb = 0
      end if

      if (g%sow_establishment .eq. 'DIRECT-SEED') then
        if (newSow%nplds .gt. 0) then
	      g%sow_nplds = newSow%nplds
        else	  
          found = ReadParam('nplds', '()', 0, g%sow_nplds, 0.0, 500.0)
	    end if
	    write (line, *) 'Number of plants direct seeded (pl/m^2) = ' , g%sow_nplds
        call writeLine (line)
      else
        g%sow_nplsb = 0
      end if

      found = ReadParam('etmethod', '()', 1, g%etmod)
      call upperc(g%etmod)
      IF (g%etmod .ne.' ' .and. g%ETMOD.EQ.'INTERNAL PENMAN') THEN
         g%ETMOD = 'PENMAN'
         call writeLine (' Evapotranspiration is (internal) Penman')
         found = ReadParam('anga', '()', 0, g%anga, 0.0, 1.0)
         found = ReadParam('angb', '()', 0, g%angb, 0.0, 1.0)
      ELSE IF (g%etmod .ne.' ' .and. g%ETMOD.EQ.'INTERNAL MAKKINK') THEN
         g%ETMOD = 'MAKKINK'
         call writeLine (' Evapotranspiration is (internal) Makkink')
      ELSE IF (g%etmod .ne.' ' .and. g%ETMOD.EQ.'INTERNAL PRIESTLEY TAYLOR') THEN
         g%ETMOD = 'PRIESTLEY TAYLOR'
         call writeLine (' Evapotranspiration is (internal) Priestley Taylor')
      ELSE IF (g%etmod .eq.' ') THEN
         call writeLine (" ETMETHOD is undefined. Using 'EO' for evapotranspiration.")
		 g%etmod = 'eo'
      ELSE
         call writeLine (' Evapotranspiration is '//trim(g%etmod))
      endif
	  
      call CallOryza(SEASINIT)
      g%sow_today = .true.
      g%hasInitialised = .true.
      g%plant_status = 'alive'

      call CallOryza(RATE)
      ! call CallOryza(INTEGR) - not required - most planting is done on prepare or process. So the integration will be called later
      return
      end subroutine

!     ===========================================================
      subroutine Oryza2OnTransplant ()
!     ===========================================================
      use Infrastructure2
      USE ModuleDefs
      Use Oryza2Module
      implicit none
!STDCALL(Oryza2OnTransplant)

!+  Purpose
!     Start the crop
      call publishNull('transplanting')

	  call fatal_error(err_user, "FIXME - not sure how to do transplanting")

      !call CallOryza(SEASINIT)
      !g%sow_today = .true.
      !g%hasInitialised = .true.
      !
      !g%plant_status = 'alive'
      return
      end subroutine


!     ===========================================================
      subroutine Oryza2OnHarvest (harvest)
!     ===========================================================
      USE ModuleDefs
      Use Oryza2Module
      implicit none
!STDCALL(Oryza2OnHarvest)
      type(harvestType) :: harvest

!+  Purpose
!     Harvest the crop
      call writeLine('Harvesting')
      call publishNull('harvesting')
	  g%harvestFraction = harvest%remove

      return
      end subroutine

!     ===========================================================
      subroutine Oryza2OnEndCrop ()
!     ===========================================================
      USE ModuleDefs
      Use Oryza2Module
      implicit none
!STDCALL(Oryza2OnEndCrop)

!+  Purpose
!     End the crop

      call CallOryza(SEASEND)    
      g%hasInitialised = .false.
      call oryza2clearoutputs ()
      call oryza2_zero_variables ()
      return
      end subroutine

      
!     ===========================================================
      subroutine Oryza2OnPrepare()
!     ===========================================================
      Use Oryza2Module
      implicit none
!STDCALL(Oryza2OnPrepare)

      g%dlt_surfaceom = 0.0      
      g%dlt_surfaceom_n = 0.0      
      g%dlt_root_mass = 0.0      
      g%dlt_root_mass_n = 0.0      

      g%dlt_sw_dep(:) = 0.0
      g%dlt_no3(:) = 0.0
      g%dlt_nh4(:) = 0.0
      g%harvestFraction = 0.0
      
      return
      end subroutine
      
!     ===========================================================
      subroutine Oryza2OnProcess()
!     ===========================================================
      USE ModuleDefs
      Use Oryza2Module
      implicit none
!STDCALL(Oryza2OnProcess)

      call CallOryza(RATE)
      return
      end subroutine

!     ===========================================================
      subroutine Oryza2OnPost()
!     ===========================================================
      USE ModuleDefs
      Use Oryza2Module
      implicit none
!STDCALL(Oryza2OnPost)

      call CallOryza(INTEGR)
      return
      end subroutine

!     ===========================================================
      subroutine Main (Action, Data_string)
!     ===========================================================
      Use Oryza2Module
      implicit none

!+  Sub-Program Arguments
      character  Action*(*)            ! Message action to perform
      character  Data_string*(*)       ! Message data

!+  Purpose
!      This routine is the interface between the main system and the
!      oryza2 module.

!+  Changes

!+  Constant Values
      character  my_name*(*)
      parameter (my_name = 'Oryza2')

!- Implementation Section ----------------------------------

      return
      end subroutine
             

!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use Oryza2Module
      implicit none
!STDCALL(alloc_dealloc_instance)
!STDCALL(Oryza2createoutputs)
!STDCALL(Oryza2closeoutputs)

!+  Sub-Program Arguments
      logical, intent(in) :: doAllocate

!+  Purpose
!      Module instantiation routine.
      integer  initCount
      data initCount /0/ 

!- Implementation Section ----------------------------------
      if (doAllocate) then
         initcount = initcount + 1
         if (initCount .gt. 1) then
            write (*,*) 'ORYZA2 cannot do multiple instances. See documentation.'
            call exit(1)
         endif
         allocate(g)
         call Oryza2createoutputs()
      else
         initcount = initcount - 1
         call Oryza2closeoutputs()
         deallocate(g)
      end if
      return
      end subroutine
