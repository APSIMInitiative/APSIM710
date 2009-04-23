*====================================================================
      real function micromet_AerodynamicConductanceOLD (
     :                         WindSpeed
     :                       , MeasurementHeight
     :                       , CropHeight
     :                       , CropLAI)
*====================================================================

      Use Infrastructure
      implicit none


*+  Sub-Program Arguments
      real Windspeed           !m/s
      REAL MeasurementHeight   !m
      real CropHeight          !m
      REAL CropLAI             !m

*+  Purpose
*     Calculate the Aerodynamic Conductance

*+  Notes

*+  Changes
*       060599 - VOS specified and programmed

*+  Calls


*+  Local Variables
      REAL ZeroPlaneDispl       !zero-plance displacement (m)
      REAL RoughnessLength      !roughness length (m)
      real FrictionVelocity     !u_star (m/s)

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_AerodynamicConductance')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      ZeroPlaneDispl = micromet_ZeroPlaneDispl(CropHeight, CropLAI)
      RoughnessLength = micromet_RoughnessLength(CropHeight,
     :                                         ZeroPlaneDispl)
      FrictionVelocity =
     :            micromet_FrictionVelocity (WindSpeed
     :                                     , MeasurementHeight
     :                                       , ZeroPlaneDispl
     :                                   , RoughnessLength)
      micromet_AerodynamicConductance =
     :        divide( FrictionVelocity**2, WindSpeed, 0.0)

      micromet_AerodynamicConductance =
     :        l_bound(micromet_AerodynamicConductance, 0.001)

      call pop_routine (myname)

      return
      end function

*====================================================================
      real function micromet_AerodynamicCondSub (
     :                                        WindSpeed
     :                                      , WindAttenuation
     :                                      , LayerLAI
     :                                      , topHeight
     :                                      , SourceAbove
     :                                      , SourceSub)
*====================================================================

      Use Infrastructure
      implicit none


*+  Sub-Program Arguments
      real WindSpeed               !wind speed (m/s)
      real WindAttenuation         !wind attenutation coefficient (-) usually 0.5
      real LayerLAI                !LAI of this canopy sub-surface layer (m)
      real TopHeight               !height of Top canopy layer (m)
      real SourceAbove             !middle of layer above(m)
      real SourceSub               !middle of this layer (m)

*+  Purpose
*     Calculate the Aerodynamic Conductance of a sub-canopy layer

*+  Notes

*+  Changes
*       110500 - VOS specified and programmed

*+  Calls


*+  Local Variables
      real ZeroPlaneDispl       !zero-plance displacement (m)
      real RoughnessLength      !roughness length (m)
      real FrictionVelocity     !u_star (m/s)
      real Diffusivity          !diffusivity function for vapour transport
      real tempAbove            !temporary variable for above layer
      real tempSub              !temporary variable for sub layer
      real RefHeight

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_AerodynamicCondSub')

      real       von_karman
      parameter (von_karman = 0.41)

*- Implementation Section ----------------------------------

      call push_routine (myname)

      RefHeight = TopHeight + 2.0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! Calculate site properties
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ZeroPlaneDispl =
     :        micromet_ZeroPlaneDispl (CropHeight, CropLAI)
      RoughnessLength =
     :        micromet_RoughnessLength(CropHeight, ZeroPlaneDispl)
      FrictionVelocity =
     :        micromet_FrictionVelocity (WindSpeed
     :                                 , RefHeight
     :                                 , ZeroPlaneDispl
     :                                 , RoughnessLength)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! Calculate diffusivity
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      Diffusivity =  von_karman**2
     :            * (CropHeight - ZeroPlaneDispl)
     :            / log ((RefHeight-ZeroPlaneDispl)/RoughnessLength) !natural log

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! Calculate conductance
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      tempTop = -1.0 * WindAttenuation
     :        * SourceTop
     :        / (2.0 * CropHeight)
      tempSub = -1.0 * WindAttenuation
     :        * SourceSub
     :        / (2.0 * CropHeight)

      micromet_AerodynamicCondSub =
     :              CropHeight
     :           *  exp(WindAttenuation)
     :           /  Diffusivity
     :           /  WindAttenuation
     :           * (exp(tempSub) - exp(tempTop))
      micromet_AerodynamicCondSub =
     :          Divide (1.0, micromet_AerodynamicCondSub, 0.0)

      call pop_routine (myname)

      return
      end function


*====================================================================
      real function micromet_RefWindSpeed (
     :                         WindSpeed
     :                       , MetHeight
     :                       , MetCropLAI
     :                       , MetCropHeight
     :                       , CropHeight
     :                       , CropLAI
     :                       , RefHeight)
*====================================================================

      Use Infrastructure
      implicit none


*+  Sub-Program Arguments
      real Windspeed               !m/s
      real MetHeight              !m
      real MetCropLAI             !m2/m2
      real MetCropHeight          !m
      real CropHeight              !m
      real CropLAI                 !m
      real RefHeight               !m

*+  Purpose
*     Correct windspeed for measurement and simulation crop characteristics

*+  Notes

*+  Changes
*       240300 - VOS specified and programmed

*+  Calls


*+  Local Variables
      real MetZeroPlaneDispl       !zero-plance displacement (m)
      real MetRoughnessLength      !roughness length (m)
      real MetFrictionVelocity     !u_star (m/s)
      real RecalcHeight             !height where wind unaffected by vegetation
      real RecalcWindSpeed          !wind speed at RecalcHeight
      real RefZeroPlaneDispl        !zero-plance displacement (m)
      real RefRoughnessLength       !roughness length (m)
      real RefFrictionVelocity      !u_star (m/s)
      real NewWindSpeed             !temporary variable

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_RefWindSpeed')

      real       von_karman
      parameter (von_karman = 0.41)

*- Implementation Section ----------------------------------

      call push_routine (myname)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! Calculate properties of the measurement site
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      MetZeroPlaneDispl = micromet_ZeroPlaneDispl(MetCropHeight,
     :                                             MetCropLAI)
      MetRoughnessLength = micromet_RoughnessLength(MetCropHeight,
     :                                               MetZeroPlaneDispl)
      MetFrictionVelocity =
     :            micromet_FrictionVelocity (WindSpeed
     :                                     , MetHeight
     :                                     , MetZeroPlaneDispl
     :                                     , MetRoughnessLength)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! Calculate height/properties where wind speed not affected by vegetation
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      RecalcHeight    = max (MetCropHeight, CropHeight) + 20.0
      RecalcWindSpeed = (RecalcHeight - MetZeroPlaneDispl)
     :                / MetRoughnessLength
      RecalcWindSpeed = max(RecalcWindSpeed, 1.0) !i.e. WindSpeed is ln(1)=0
      RecalcWindSpeed = von_karman / MetFrictionVelocity
     :                * log(RecalcWindSpeed)  !natural log

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! Calculate properties of the simulation/reference site
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      RefZeroPlaneDispl = micromet_ZeroPlaneDispl(CropHeight,
     :                                            CropLAI)
      RefRoughnessLength = micromet_RoughnessLength(CropHeight,
     :                                              RefZeroPlaneDispl)
      RefFrictionVelocity =
     :            micromet_FrictionVelocity (RecalcWindSpeed
     :                                     , RecalcHeight
     :                                     , RefZeroPlaneDispl
     :                                     , RefRoughnessLength)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! Calculate reference wind speed to be used in further calculations
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      NewWindSpeed = (RefHeight - RefZeroPlaneDispl)
     :             / RefRoughnessLength
      NewWindSpeed = max(NewWindSpeed, 1.0) !i.e. WindSpeed is zero
      NewWindSpeed = von_karman / RefFrictionVelocity
     :             * log(NewWindSpeed)      !natural log

      micromet_RefWindSpeed = NewWindSpeed

      call pop_routine (myname)

      return
      end function

*====================================================================
      real function micromet_GashIntercep (
     :                Precip
     :              , PrecipDurn
     :              , FreeEvapRate
     :              , MaxStorage
     :              , PropThroughfall)
*====================================================================

      Use Infrastructure
      implicit none


*+  Sub-Program Arguments
      real    Precip              !mm irrigation or rainfall
      real    PrecipDurn          !sec, duration of rainfall or irrigation
      real    FreeEvapRate        !mm/sec
      real    MaxStorage          !mm water stored / unit LAI
      real    PropThroughfall     !propportioin of precipitation that falls though the canopy

*+  Purpose
*     calculate the amount (mm) of rainfall or irrigation
*     interception according ot the Gash model

*+  Notes

*+  Changes
*       160799 - VOS specified and programmed

*+  Calls

*+  Local Variables
      REAL temp                      !temprary variable
      REAL PrecipRate                !rainfall or irrigation rate, mm/s
      REAL MaxIntercep               !maximum possible interception, mm
      REAL Precip2Sat                !mm of prcipitation to saturate the canopy, mm

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_GashIntercep')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      PrecipRate = Divide(Precip, PrecipDurn, 1.0) !set top 1.0 mm/s

      MaxIntercep = (1.0 - PropThroughfall) * Precip

      FreeEvapRate = l_bound(FreeEvapRate, 1e-6)

      if (PrecipRate .le. FreeEvapRate) then
         micromet_GashIntercep = MaxIntercep  !everything that lands evaporates
      elseif (PropThroughfall .gt. 0.999) then
         micromet_GashIntercep = 0.0          !nothing is intercepted
      else                       !go and calculate interception
         temp = 1.0 - (FreeEvapRate/PrecipRate)/(1.0-PropThroughfall)
         if (temp .le. 0.0) then
            micromet_GashIntercep = MaxIntercep
         else
            Precip2Sat = -PrecipRate / FreeEvapRate
     :                 * MaxStorage * log(temp)
            micromet_GashIntercep =
     :                  (1-PropThroughfall) * Precip2Sat
     :                + FreeEvapRate / PrecipRate * (Precip-Precip2Sat)
         end if
      end if

      micromet_GashIntercep
     :        = bound(micromet_GashIntercep, 0.0, MaxIntercep)

      call pop_routine (myname)

      return
      end function
*====================================================================
      real function micromet_Intercep (
     :                CropK
     :              , CropLAI
     :              , CropSpecificStorage
     :              , PropCanopyWetted
     :              , EmmisCanopy
     :              , Precip
     :              , PrecipDurn
     :              , mint
     :              , maxt
     :              , vp
     :              , AirPressure
     :              , AerodynamicCond
     :              , Sun_Angle)
*====================================================================

      Use Infrastructure
      implicit none


*+  Sub-Program Arguments
       real CropK                !crop extinction coefficient
       real CropLAI              !crop LAI
       real CropSpecificStorage  !mm water store per unit LAI
       real PropCanopyWetted     !for irrigation below canopy
       real EmmisCanopy          !usually 0.96
       real Precip               !amount of rainfall or irrigation, mm
       real PrecipDurn           !duration of precipitation, sec
       real mint                 !minimum temperature  C
       real maxt                 !maximum temperature  C
       real vp                   !humidity  hPa = mbar
       real AirPressure          !air pressure hPa
       real AerodynamicCond      !m/s
       REAL Sun_Angle

*+  Purpose
*

*+  Notes

*+  Changes
*       160799 - VOS specified and programmed

*+  Calls


*+  Local Variables
      REAL PropThroughfall                      !
      REAL MaxStorage           !
      REAL FreeEvaprate

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_Intercep')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      PropThroughfall = micromet_PropThroughfall (
     :                                CropK
     :                              , CropLAI)
      MaxStorage = micromet_MaxStorage (
     :                                CropSpecificStorage
     :                              , CropLAI
     :                              , PropCanopyWetted)
      FreeEvapRate = micromet_PenmanMonteith (
     :                -35.0         ! latitude not used
     :              , 2             ! day not used
     :              , 1440.0        ! timestep not used
     :              , Sun_Angle
     :              , 0.0           ! SunshineHrs
     :              , 0.0           ! radn not used
     :              , 0.15          ! albedo not used
     :              , EmmisCanopy
     :              , mint
     :              , maxt
     :              , vp
     :              , AirPressure
     :              , AerodynamicCond
     :              , 0.0)          ! CanopyCond is zero - free water
     :              / (day_length (2, -35.0, Sun_Angle) * 3600.0)

      micromet_Intercep = micromet_GashIntercep (
     :                               Precip
     :                             , PrecipDurn
     :                             , FreeEvapRate
     :                             , MaxStorage
     :                             , PropThroughfall)

      call pop_routine (myname)

      return
      end function

*====================================================================
      real function micromet_PropThroughfall (
     :               CropK
     :             , CropLAI)
*====================================================================

      Use Infrastructure
      implicit none


*+  Sub-Program Arguments
      real    CropK                  !crop exctinction coefficient
      REAL    CropLAI                !crio leaf area index

*+  Purpose
*     calculate the proportion of precipitation falling through
*     the canopy

*+  Notes
*     assumes that the light extinction coeffient can be applied to
*     water

*+  Changes
*       160799 - VOS specified and programmed

*+  Calls

*+  Local Variables

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_PropThroughfall')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      micromet_PropThroughfall = EXP (-1 * CropK * CropLAI)

      call pop_routine (myname)

      return
      end function
*====================================================================
      real function micromet_MaxStorage (
     :                CropSpecificStorage
     :              , CropLAI
     :              , PropCanopyWetted)
*====================================================================

      Use Infrastructure
      implicit none


*+  Sub-Program Arguments
      real    CropSpecificStorage     !mm water stored per uni LAI
      REAL    CropLAI                 !leaf area index
      REAL    PropCanopyWetted        !1.0 for rainfall, maybe < 1 for irrigation

*+  Purpose
*    calculated the maximum amount of water that can be stored on
*    the leaves corrected for irrigation height relative to canopy
*    height if necessary

*+  Notes

*+  Changes
*       160799 - VOS specified and programmed

*+  Calls

*+  Local Variables

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_MaxStorage')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      micromet_MaxStorage = CropSpecificStorage
     :                    * CropLAI
     :                    * PropCanopyWetted

      micromet_MaxStorage = L_BOUND(micromet_MaxStorage, 0.0)

      call pop_routine (myname)

      return
      end function

*====================================================================
      real function micromet_InterpTemp (
     :                time
     :              , maxt_time
     :              , mint
     :              , maxt
     :              , mint_yesterday
     :              , maxt_yesterday)
*====================================================================

      Use Infrastructure
      implicit none


*+  Sub-Program Arguments
      real time           !time of day in hours
      REAL maxt_time      !time of day for minimum temperature, hours
      REAL mint           !minimum temperature, C
      REAL maxt           !maximum temperature, C
      REAL mint_yesterday !minimum temperature yesterady, C
      REAL maxt_yesterday !maximum temperature yesterady, C

*+  Purpose
*    Interpolate air temperature

*+  Notes
*    Between midinight and mint_time just a linear interpolation between
*    yesterday's midnight temperature and today's mint.  For the rest of
*    the day use a sin function

*+  Changes
*       290799 - VOS specified and programmed

*+  Calls

*+  Local Variables
      REAL p_time               !time as proportion of the day
      REAL p_maxt_time          !mint_time as proportion of the day
      REAL p_mint_time          !mint_time as proportion of the day
      REAL t_midnight           !temperature last midnight
      REAL t_scale              !0 at midnight, 1 at mint_time

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_InterpTemp')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      p_time = time / 24.0
      p_maxt_time = maxt_time / 24.0
      p_mint_time = p_maxt_time - 0.5

      if (p_time .lt. p_mint_time) then
         t_midnight = SIN((0.0 + 0.25 - p_maxt_time)*2.0*pi)
     :              * (maxt_yesterday - mint_yesterday) / 2.0
     :              + (maxt_yesterday + mint_yesterday) / 2.0
         t_scale = (p_mint_time - p_time) / p_mint_time
         t_scale = bound(t_scale, 0.0, 1.0)
         micromet_InterpTemp = mint + t_scale*(t_midnight-mint)
      else
         micromet_InterpTemp = SIN((p_time + 0.25 - p_maxt_time)*2.0*pi)
     :                       * (maxt - mint) / 2.0
     :                       + (maxt + mint) / 2.0
      end if

      call pop_routine (myname)

      return
      end function

*====================================================================
      real function micromet_PM_PropRad (
     :               latitude
     :              ,day
     :              ,timestep
     :              ,Sun_Angle
     :              ,SunshineHrs
     :              ,radn
     :              ,albedo
     :              ,EmmisCanopy
     :              ,mint
     :              ,maxt
     :              ,vp
     :              ,AirPressure
     :              ,AerodynamicCond
     :              ,CanopyCond)
*====================================================================

      Use Infrastructure
      implicit none


*+  Sub-Program Arguments
      real    latitude            !decimal degrees
      integer day                 !day of year
      real    albedo              !of the crop
      real    radn                !MJ/m2
      real    mint                !degC
      real    maxt                !degC
      real    vp                  !humidity in hPa
      real    AerodynamicCond     !m/s
      real    CanopyCond          !m/s
      real    SunshineHrs         !length of bright sunshine
      REAL    EmmisCanopy         !emmisivity of the canopy -
      REAL    AirPressure         !hPa
      REAL    timestep            !minutes
      REAL    Sun_Angle

*+  Purpose
*     Calculate the Penman-Monteith water demand

*+  Notes

*+  Changes
*       050799 - VOS specified and programmed

*+  Calls


*+  Local Variables
      REAL Non_dQs_dT
      REAL SolRad               !short wave radiation W/m2
      REAL LongWave             !W/m2
      REAL NetRadiation
      REAL RhoA
      REAL Lambda
      REAL SpecificVPD
      REAL RadiationTerm
      REAL AerodynamicTerm
      real DayLength           !length of day (hours)
      REAL denominator         !of the Penman-Monteith equation

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_PM_PropRad')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      if (NINT(timestep/60.0) .eq. 24 ) then
         DayLength = day_length (day,latitude, Sun_Angle)
      else
         DayLength = timestep/60.0   !convert from min to hours
      end if

      Non_dQs_dT = micromet_Non_dQs_dT ((mint+maxt )/2.0 ,AirPressure)
      SolRad = micromet_Radn2SolRad (
     :               latitude
     :              ,day
     :              ,timestep
     :              ,Sun_Angle
     :              ,radn)
      LongWave = micromet_longwave (   (mint+maxt )/2.0
     :                                ,SunshineHrs/DayLength
     :                                ,EmmisCanopy)
      NetRadiation = (1-Albedo) * SolRad + LongWave

      RhoA = micromet_RhoA ((mint + maxt)/2.0, AirPressure)
      Lambda = micromet_Lambda((mint + maxt)/2.0)
      SpecificVPD = micromet_SpecificVPD (
     :                                     vp
     :                                   , mint
     :                                   , maxt
     :                                   , AirPressure)

      Denominator = Non_dQs_dT
     :            + Divide( AerodynamicCond, CanopyCond, 0.0)
     :            + 1.0

      RadiationTerm =
     :          Divide( Non_dQs_dT * NetRadiation
     :                , Denominator, 0.0)
     :                * 1000.0 * (DayLength *3600.0)
     :                / Lambda / RhoW
      AerodynamicTerm =
     :          Divide( RhoA * Lambda* SpecificVPD * AerodynamicCond
     :                , Denominator, 0.0)
     :                * 1000.0 * (DayLength *3600.0)
     :                / Lambda / RhoW

      micromet_PM_PropRad = Divide(
     :                      RadiationTerm
     :                    , RadiationTerm + AerodynamicTerm
     :                    , 0.0)

      call pop_routine (myname)

      return
      end function

*====================================================================
      real function micromet_SatSpecificHumidity (mint
     :                                           ,maxt
     :                                           ,AirPressure)
*====================================================================

      Use Infrastructure
      implicit none


*+  Sub-Program Arguments
      real mint            !degC
      real maxt            !degC
      REAL AirPressure     !hPa

*+  Purpose
*     calculate saturated specific humidity from temperature

*+  Notes

*+  Changes
*       050799 - VOS specified and programmed

*+  Calls

*+  Local Variables
      REAL SatVP        !saturated vapour pressure in hPa

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_SatSpecificHumidity')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      SatVP = (micromet_svp(mint)+micromet_svp(maxt))/ 2.0

      micromet_SatSpecificHumidity = (mwh2o/mwair) / AirPressure
     :                             * SatVP

      call pop_routine (myname)

      return
      end function
      