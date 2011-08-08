
*====================================================================
      real function micromet_PenmanMonteith (
     :               latitude
     :              ,day
     :              ,timestep
     :              ,Sun_angle
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
      REAL    Sun_Angle           !Sun position for start of day

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
      REAL AverageT

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_PenmanMonteith')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      if (NINT(timestep/60.0) .eq. 24 ) then
         DayLength = day_length (day,latitude, sun_angle)
      else
         DayLength = timestep/60.0   !convert from min to hours
      end if

      AverageT = micromet_averageT(mint,maxt)

      Non_dQs_dT = micromet_Non_dQs_dT (AverageT ,AirPressure)
      SolRad = micromet_Radn2SolRad (
     :               latitude
     :              ,day
     :              ,timestep
     :              ,sun_angle
     :              ,radn)
      LongWave = micromet_longwave (   AverageT
     :                                ,SunshineHrs/DayLength
     :                                ,EmmisCanopy)
      NetRadiation = (1-Albedo) * SolRad + LongWave

      RhoA = micromet_RhoA (AverageT, AirPressure)
      Lambda = micromet_Lambda(AverageT)
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

      micromet_PenmanMonteith = RadiationTerm + AerodynamicTerm

      call pop_routine (myname)

      return
      end function

*====================================================================
      real function micromet_Omega (
     :               mint
     :              ,maxt
     :              ,AirPressure
     :              ,AerodynamicCond
     :              ,CanopyCond)
*====================================================================

      implicit none


*+  Sub-Program Arguments
      real    mint                !degC
      real    maxt                !degC
      real    AerodynamicCond     !m/s
      real    CanopyCond          !m/s
      REAL    AirPressure         !hPa

*+  Purpose
*     Calculate the Jarvis & McNaughton decoupling coefficient, omega

*+  Notes

*+  Changes
*       050799 - VOS specified and programmed

*+  Calls

*+  Local Variables
      REAL Non_dQs_dT

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_Omega')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      Non_dQs_dT = micromet_Non_dQs_dT ((mint+maxt )/2.0 ,AirPressure)

      micromet_Omega =
     :   Divide( Non_dQs_dT + 1.0
     :         , Non_dQs_dT + 1.0 + Divide( AerodynamicCond
     :                                    , CanopyCond
     :                                    , 0.0)
     :         , 0.0)

      call pop_routine (myname)

      return
      end function
*====================================================================
      real function micromet_ActualCanopyCond (
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
     :              ,Transpiration)
*====================================================================

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
      real    Transpiration       !mm
      real    SunshineHrs         !length of bright sunshine
      REAL    EmmisCanopy         !emmisivity of the canopy -
      REAL    AirPressure         !hPa
      REAL    timestep            !minutes
      REAL    Sun_Angle

*+  Purpose
*     given the actual water use, calculate the
*     effective canopy conductance

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
      real DayLength           !length of day (hours)
      REAL denominator         !of the Penman-Monteith equation
      real LambdaE             !transpiration time specific heat

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_ActualCanopyCond')

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

      LambdaE = Transpiration
     :                * 1000.0 * (DayLength *3600.0)
     :                / Lambda / RhoW

      Denominator = Non_dQs_dT * NetRadiation
     :            + Lambda * RhoA * SpecificVPD * AerodynamicCond
     :            - LambdaE * Non_dQs_dT
     :            - LambdaE


      micromet_ActualCanopyCond =
     :          Divide( AerodynamicCond * LambdaE
     :                , Denominator
     :                , 0.0)

      call pop_routine (myname)


      return
      end function
*====================================================================
      real function micromet_RhoA (temperature
     :                            ,AirPressure)
*====================================================================

      implicit none

*+  Sub-Program Arguments
      real       temperature           ! (INPUT) temperature (oC)
      real       AirPressure           ! (INPUT) air pressure (hPa)

*+  Purpose
*     calculate the density of air (kg/m3) at a given temperature
*     and pressure

*+  Changes
*       291098 - NIH adapted from grandis module
*       020799 - VOS checked units

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_RhoA')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      micromet_RhoA = divide(
     :                 mwair
     :              *  AirPressure * 100.  !air pressure converted to Pa
     :              , (abs_temp + temperature)
     :              * r_gas
     :              , 0.0)

      call pop_routine (myname)
      return
      end function

*====================================================================
      real function micromet_Lambda (temperature)
*====================================================================
      implicit none


*+  Sub-Program Arguments
      real temperature   ! temperature (oC)

*+  Purpose
*     calculate the lambda (latent heat of vapourisation for water)(J/kg)

*+  Changes
*       291098 - NIH Adapted from Eo module

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_lambda')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      micromet_lambda = (2501.0 - 2.38 * temperature)*1000.0      ! J/kg

      call pop_routine (myname)
      return
      end function

*====================================================================
      real function micromet_svp (temperature)
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
      parameter (myname = 'micromet_svp')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      micromet_svp = svp_A*exp(svp_B*temperature/(temperature + svp_C))

      call pop_routine (myname)
      return
      end function

*====================================================================
      real function micromet_Non_dQs_dT    (temperature
     :                                     ,air_pressure)
*====================================================================

      implicit none


*+  Sub-Program Arguments
      real       temperature    ! (INPUT) minimum temperature (oC)
      real       air_pressure   ! (INPUT) air pressure (hPa)

*+  Purpose
*     calculate Non_dQs_dT - the dimensionless value for
*     d(sat spec humidity)/dT ((kg/kg)/K) FROM TETEN FORMULA

*+  Changes
*       291098 - NIH adapted from Eo module

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_Non_dQs_dT')

*+  Local Variables
      real       desdt                 ! d(sat VP)/dT: (mb/K)
      real       dqsdt                 ! d(sat spec hum)/dT: (kg/kg)/K
      real       esat                  ! saturated vapour pressure (mb)

*- Implementation Section ----------------------------------

      call push_routine (myname)

      esat = micromet_svp (temperature)  !saturated vapour pressure

      desdt = esat*svp_B*svp_C/ (svp_C + temperature)**2   ! d(sat VP)/dT: (mb/K)
      dqsdt = (mwh2o/mwair) *desdt/air_pressure            ! d(sat spec hum)/dT: (kg/kg)/K

      micromet_Non_dQs_dT = micromet_lambda (temperature)/Cp *dqsdt    ! dimensionless

      call pop_routine (myname)
      return
      end function

*====================================================================
      real function micromet_longwave (temperature
     :                                ,FracClearSkyRad
     :                                ,emmis_canopy)
*====================================================================

      implicit none

*+  Sub-Program Arguments
      real temperature    ! temperature (oC)
      real FracClearSkyRad ! R/Ro, SunshineHrs/DayLength (0-1)
      real emmis_canopy   ! canopy emmissivity

*+  Purpose
*     calculate the net longwave radiation 'in' (W/m2)

*+  Notes
*   Emissivity of the sky comes from Swinbank, W.C. (1963).
*   Longwave radiation from clear skies Quart. J. Roy. Meteorol.
*   Soc. 89, 339-348.

*+  Changes
*       291098 - NIH Adapted from Grandis module
*       050799 - VOS Changed sign so that is net INWARDS longwave
*       060799 - VOS Changed arguments from sunhine hours and daylength
*           to FracClearSkyRadn for compatability with variable timestep


*+  Local Variables
      real emmis_sky    ! emmisivity of the sky
      real cloud_effect ! cloud effect on net long wave (0-1)

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_longwave')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      emmis_sky = 9.37e-6*(temperature+abs_temp)**2
      ! assume constant value for now
      !emmis_sky = 0.80


      FracClearSkyRad = bound(FracClearSkyRad, 0.0, 1.0)
      cloud_effect = (c_cloud + (1.0-c_cloud)*FracClearSkyRad)

      ! remove cloud effect for now
      !cloud_effect = 1.0

      micromet_longwave = cloud_effect
     :                  * (emmis_sky - emmis_canopy)
     :                  * stef_boltz*(temperature+abs_temp)**4

      ! Try Monteith approach
      !micromet_longwave = -(107. - 0.3 * temperature)

      call pop_routine (myname)
      return
      end function
*====================================================================
      real function micromet_DayLength      (latitude
     :                                      ,day
     :                                      ,Sun_angle)
*====================================================================

      implicit none


*+  Sub-Program Arguments
      real    latitude
      integer day       !day of year
      REAL    Sun_Angle

*+  Purpose
*     calculate the day length from latitude, and day of year
*     - only usedfor Excel

*+  Notes

*+  Changes
*       050799 - VOS specified and programmed

*+  Local Variables

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_DayLength')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      micromet_DayLength = day_length (day,latitude, Sun_Angle)

      call pop_routine (myname)

      return
      end function
*====================================================================
      real function micromet_sunshine_hours (radn
     :                                      ,daylengthlight
     :                                      ,latitude
     :                                      ,day)
*====================================================================

      implicit none


*+  Sub-Program Arguments
      real    radn      !shortwave radiation from met file
      real    latitude
      real    DayLengthLight
      integer day       !day of year

*+  Purpose
*     calculate the number of sunshine hours from shortwave radiation
*     latitude, and day of year

*+  Notes
*     Assume that sunshine hours is the day length times measured
*     shortwave radiation divided by maximum possible shortwave
*     radiation.  Scheme for max. possible radiation is taken from:
*     Smith, M. R., Allen, R. G., Monteith, J. L., Perrier, A.,
*             Satos Pereira, L., and Segeren, A. (1992). Expert
*             consultation on revision of FAO methodologies for
*             crop water requirements. Land and Water Division,
*             Food and Agriculture Organization of the United
*             Nations, Rome.
*     and
*     Grayson, R. B., Argent, R. M., Nathan, R. J.,
*             McMahon, T. A., and Mein, R. G. (1996). 'Hydrological
*             Recipes: Estimation Techniques in Australian Hydrology.'
*             (Cooperative Research Center for Catchment Hydrology:
*             Clayton, Victoria, Australia.)

*+  Changes
*       020799 - VOS specified and programmed

*+  Local Variables
      real MaxSunHrs           !maximum possible number of sunshine hours (i.e. daylength)
      real RelativeDistance    !relative distance between sun and earth
      real SolarDeclination
      real SunsetAngle
      real ExtraTerrestrialRadn
      real MaxRadn             !maximum possible shortwave radiation

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_sunshine_hours')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      MaxSunHrs = DayLengthLight

      RelativeDistance = 1.0 + 0.033 * cos(0.0172 * day)

      SolarDeclination = 0.409 * sin(0.0172 * day - 1.39)

      SunsetAngle = acos(-tan(latitude * Deg2Rad)
     :                   *tan(SolarDeclination))

      ExtraTerrestrialRadn = 37.6 * RelativeDistance *
     :        ( SunsetAngle
     :        * sin(latitude * Deg2Rad)
     :        * sin(SolarDeclination)
     :        +
     :          cos(latitude * Deg2Rad)
     :        * cos(SolarDeclination)
     :        * sin(SunsetAngle))

      MaxRadn = 0.75 * ExtraTerrestrialRadn

!finally calculate the sunshine hours as the ratio of
!maximum possible radiation
      micromet_sunshine_hours = min(MaxSunHrs * Radn / MaxRadn,
     :                              MaxSunHrs)

      call pop_routine (myname)

      return
      end function
*====================================================================
      real function micromet_SpecificHumidity (vp
     :                                        ,AirPressure)
*====================================================================

      implicit none


*+  Sub-Program Arguments
      real vp              !vapour pressure in hPa (=mbar)
      REAL AirPressure     !hPa

*+  Purpose
*     calculate specific humidity from vapour pressure

*+  Notes

*+  Changes
*       050799 - VOS specified and programmed

*+  Local Variables

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_SpecificHumidity')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      micromet_SpecificHumidity = (mwh2o/mwair) *vp /AirPressure

      call pop_routine (myname)

      return
      end function
*====================================================================
      real function micromet_FrictionVelocity (
     :                         WindSpeed
     :                       , MeasurementHeight
     :                       , ZeroPlaneDispl
     :                       , RoughnessLength)
*====================================================================

      implicit none


*+  Sub-Program Arguments
      real Windspeed          !m/s
      REAL MeasurementHeight  !m
      REAL ZeroPlaneDispl     !m
      REAL RoughnessLength    !m

*+  Purpose
*     Calculate the friction velocity

*+  Notes

*+  Changes
*       060599 - VOS specified and programmed

*+  Calls

*+  Local Variables
      REAL denominator    !intermediation variable (-)

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_FrictionVelocity')

      REAL       von_karman
      parameter (von_karman = 0.41)
*- Implementation Section ----------------------------------

      call push_routine (myname)

      denominator =
     :      divide( MeasurementHeight - ZeroPlaneDispl
     :            , RoughnessLength
     :            , 0.0)
      if (denominator .lt. 1.0 ) then
         micromet_FrictionVelocity = 0.0
      else
         micromet_FrictionVelocity = von_karman * Windspeed
     :                             / log(denominator)  !natural log
      end if

      call pop_routine (myname)

      return
      end function
*====================================================================
      real function micromet_ZeroPlaneDispl (CropHeight, CropLAI)
*====================================================================

      implicit none


*+  Sub-Program Arguments
      real CropHeight          !m
      REAL CropLAI             !m

*+  Purpose
*     calculate the zero-plane displacement height

*+  Notes

*+  Changes
*       060599 - VOS specified and programmed

*+  Calls

*+  Local Variables

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_ZeroPlaneDispl')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      if (CropLAI .lt. 0.001) then
         micromet_ZeroPlaneDispl = 0.0
      else
         micromet_ZeroPlaneDispl =
     :                    l_bound( 0.74 * CropHeight
     :                           + 0.1 * CropHeight * LOG10(CropLAI)
     :                           , 0.0)
      end if

      ! Try over-riding this
      micromet_ZeroPlaneDispl = 0.66 * CropHeight

      call pop_routine (myname)

      return
      end function
*====================================================================
      real function micromet_RoughnessLength (CropHeight,ZeroPlaneDispl)
*====================================================================

      implicit none


*+  Sub-Program Arguments
      real CropHeight          !m
      REAL ZeroPlaneDispl      !m

*+  Purpose
*     calculate the roughness length of the crop

*+  Notes

*+  Changes
*       060599 - VOS specified and programmed

*+  Calls

*+  Local Variables

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_RoughnessLength')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      micromet_RoughnessLength = 0.36 * CropHeight *
     :                 (1 - divide(ZeroPlaneDispl, CropHeight, 0.0))

      ! Try simple approach
      micromet_RoughnessLength = 0.123*CropHeight

      call pop_routine (myname)

      return
      end function
*====================================================================
      real function micromet_CanopyConductance (CropGsMax
     :                                        , CropR50
     :                                        , CropRGfac
     :                                        , CropLAIfac
     :                                        , LayerK
     :                                        , LayerLAI
     :                                        , LayerSolRad)
*====================================================================

      implicit none


*+  Sub-Program Arguments
      real CropGsMax        !crop-specific maximum stomatal conductance (m/s)
      real CropR50          !crop-specific SolRad at which stomatal conductance decreases to 50% (W/m2)
      real CropRGfac        !crop-specific relative growth stress factor (0-1)
      real CropLAIfac       !crop-specific LAI fraction of total LAI in current layer (0-1)
      real LayerK           !layer-averaged light extinction coeficient (-)
      real LayerLAI         !LAI within the current layer (m2/m2)
      real LayerSolRad      !solar radiation arriving at the top of the current layer(W/m2)

*+  Purpose
*       calculate the crop canpopy conductance

*+  Notes

*+  Changes
*       060599 - VOS specified and programmed

*+  Calls

*+  Local Variables
      real Numerator    !intermediate variable
      real Denominator  !intermediate variable
      real Hyperbolic   !intermediate variable

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_CanopyConductance')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      Numerator   =   LayerSolRad
     :            + CropR50
      Denominator = LayerSolRad
     :            * exp(-1
     :                  * LayerK
     :                  * LayerLAI)
     :            + CropR50
      Hyperbolic = divide(Numerator, Denominator, 0.0)
      Hyperbolic = L_Bound(Hyperbolic, 1.0)

      micromet_CanopyConductance =
     :       divide (CropGsMax * CropRGfac * CropLAIfac
     :              ,LayerK
     :              ,0.0)
     :       * log(Hyperbolic)  !natural log

      micromet_CanopyConductance =
     :        l_bound(micromet_CanopyConductance, 0.0001)

      call pop_routine (myname)

      return
      end function
*====================================================================
      real function micromet_Radn2SolRad (
     :               latitude
     :              ,day
     :              ,timestep
     :              ,Sun_Angle
     :              ,radn)
*====================================================================

      implicit none


*+  Sub-Program Arguments
      real    latitude            !decimal degrees
      integer day                 !day of year
      REAL    timestep            !minutes
      real    radn                !MJ/m2
      REAL    Sun_Angle

*+  Purpose
*     Convert "radn" in MJ/m2/period to solar radiation in W/m2

*+  Notes

*+  Changes
*       050799 - VOS specified and programmed

*+  Calls

*+  Local Variables
      REAL DayLength

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_Radn2SolRad')


*- Implementation Section ----------------------------------

      call push_routine (myname)

      if (NINT(timestep/60.0) .eq. 24 ) then
         DayLength = day_length (day,latitude, Sun_Angle)
      else
         DayLength = timestep/60.0   !convert from min to hours
      end if

      micromet_Radn2SolRad = radn * 10**6 / (DayLength *3600.0)    !W/m2

      call pop_routine (myname)

      return
      end function
*====================================================================
      real function micromet_FreeEvapRate (
     :               EmmisCanopy
     :              ,mint
     :              ,maxt
     :              ,vp
     :              ,AirPressure
     :              ,AerodynamicCond
     :              ,Sun_Angle)
*====================================================================

      implicit none


*+  Sub-Program Arguments
      REAL    EmmisCanopy         !emmisivity of the canopy -
      real    mint                !degC
      real    maxt                !degC
      real    vp                  !humidity in hPa
      REAL    AirPressure         !hPa
      real    AerodynamicCond     !m/s
      REAL    Sun_Angle

*+  Purpose
*     Calculate the Penman-Monteith water demand for zero
*     surface resistance (appropriate to wetted leaves)
*     Assume no sunshine during rainfall - therefore longwave radiation only

*+  Notes

*+  Changes
*       160799 - VOS specified and programmed

*+  Calls

*+  Local Variables

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_FreeEvapRate')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      micromet_FreeEvapRate = micromet_PenmanMonteith (
     :                -35.0         ! latitude not used
     :              , 2             ! day not used
     :              , 1440.0        ! timestep not used
     :              , Sun_Angle
     :              , 0.0           ! SunshineHrs, zero for longwave
     :              , 0.0           ! radn not used
     :              , 0.15          ! albedo not used
     :              , EmmisCanopy
     :              , mint
     :              , maxt
     :              , vp
     :              , AirPressure
     :              , AerodynamicCond
     :              , 0.0)          ! CanopyCond is zero - free water
     :              / (day_length (2, -35.0, Sun_Angle) * 3600.0) !mm/sec

      call pop_routine (myname)

      return
      end function


*====================================================================
      real function micromet_VPD (
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
*       230300 - VOS specified and programmed

*+  Calls

*+  Local Variables
      real       VPDmint !VPD at minimium temperature
      real       VPDmaxt !VPD at maximium temperature

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_VPD')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      VPDmint = micromet_svp(mint) - vp
      VPDmint = l_bound(VPDmint,0.0)
      VPDmaxt = micromet_svp(maxt) - vp
      VPDmaxt = l_bound(VPDmaxt,0.0)

      micromet_VPD = svp_fract * VPDmaxt
     :             + (1 - svp_fract) * VPDmint

      call pop_routine (myname)
      return
      end function

*====================================================================
      real function micromet_SpecificVPD (
     :                                   vp
     :                                 , mint
     :                                 , maxt
     :                                 , AirPressure)
*====================================================================

      implicit none


*+  Sub-Program Arguments
      real       vp             ! (INPUT) vapour pressure (hPa = mbar)
      real       mint           ! (INPUT) minimum temperature (oC)
      real       maxt           ! (INPUT) maximum temperature (oC)
      real       AirPressure    ! (INPUT) Air pressure (hPa)

*+  Purpose
*     calculate the vapour pressure deficit

*+  Changes
*       230300 - VOS specified and programmed

*+  Calls


*+  Local Variables
      real       VPD     !VPD in hPa

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_SpecificVPD')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      VPD = micromet_VPD (vp, mint, maxt)
      micromet_SpecificVPD = micromet_SpecificHumidity (VPD
     :                                                 ,AirPressure)

      call pop_routine (myname)
      return
      end function


*====================================================================
      real function micromet_AerodynamicConductance(
     :                         WindSpeed
     :                       , TopHeight
     :                       , LAItot)
*====================================================================

      implicit none


*+  Sub-Program Arguments
      real Windspeed               !m/s
      real TopHeight              !m
      real LAItot                 !m2/m2

*+  Purpose
*     Calculate the Aerodynamic Conductance for the topmost layer

*+  Notes

*+  Changes
*       240300 - VOS specified and programmed

*+  Calls


*+  Local Variables
      real ZeroPlaneDispl       !zero-plance displacement (m)
      real RoughnessLength      !roughness length (m)
      real FrictionVelocity     !u_star (m/s)
      real RefHeight

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_AerodynamicConductance')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      RefHeight = TopHeight + 2.0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! Calculate site properties
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ZeroPlaneDispl =
     :        micromet_ZeroPlaneDispl (TopHeight, LAItot)
      RoughnessLength =
     :        micromet_RoughnessLength(TopHeight, ZeroPlaneDispl)
      FrictionVelocity =
     :        micromet_FrictionVelocity (WindSpeed
     :                                 , RefHeight
     :                                 , ZeroPlaneDispl
     :                                 , RoughnessLength)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! Calculate conductance
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      micromet_AerodynamicConductance =
     :        divide( FrictionVelocity**2, WindSpeed, 0.0)
      micromet_AerodynamicConductance =
     :        l_bound(micromet_AerodynamicConductance, 0.001)

      call pop_routine (myname)

      return
      end function

*====================================================================
      real function micromet_AerodynamicConductanceFAO(
     :                         WindSpeed
     :                       , RefHeight
     :                       , TopHeight
     :                       , LAItot)
*====================================================================

      implicit none


*+  Sub-Program Arguments
      real Windspeed               !m/s
      real RefHeight
      real TopHeight              !m
      real LAItot                 !m2/m2

*+  Purpose
*     Calculate the Aerodynamic Conductance using FAO approach

*+  Notes

*+  Changes
*

*+  Calls


*+  Local Variables
      real d    ! zero plane displacement height (m)
      real hterm! heat term in Ga calculation
      real mterm! momentum term in Ga calculation
      real Zh   ! height of humidity measurement (m)
      real Z0h  ! roughness length governing transfer of heat and vapour (m)
      real Zm   ! height of wind measurement (m)
      real Z0m  ! roughness length governing transfer of momentum (m)

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_AerodynamicConductanceFAO')

      REAL       von_karman
      parameter (von_karman = 0.41)

*- Implementation Section ----------------------------------

      call push_routine (myname)



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! Calculate site properties
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      d = 0.666 * TopHeight
      Zh = TopHeight + RefHeight ! assume reference above canopy
      Zm = TopHeight + RefHeight
      Z0m = 0.123 * TopHeight
      Z0h = 0.1*Z0m

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! Calculate conductance
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      if ((Z0m.ne.0).and.(Z0h.ne.0)) then

         mterm = divide (von_karman
     :                  ,log(divide(Zm-d,Z0m,0.0))
     :                  ,0.0)
         hterm = divide (von_karman
     :                  ,log(divide(Zh-d,Z0h,0.0))
     :                  ,0.0)
      else
         mterm = 0.0
         hterm = 0.0
      endif

      micromet_AerodynamicConductanceFAO =
     :        WindSpeed*mterm*hterm
      micromet_AerodynamicConductanceFAO =
     :        l_bound(micromet_AerodynamicConductanceFAO, 0.001)

      call pop_routine (myname)

      return
      end function

*====================================================================
      real function micromet_AverageT (
     :                          mint
     :                        , maxt)
*====================================================================

      implicit none


*+  Sub-Program Arguments
      real       mint           ! (INPUT) minimum temperature (oC)
      real       maxt           ! (INPUT) maximum temperature (oC)

*+  Purpose
*     calculate the vapour pressure deficit

*+  Changes
*       230300 - VOS specified and programmed

*+  Calls

*+  Local Variables


*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_AverageT')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      micromet_AverageT = 0.75 * maxt + 0.25 * mint

      call pop_routine (myname)
      return
      end function

*====================================================================
      real function micromet_Soil_Heat_Flux (
     :                          Radn
     :                        , RadnInt
     :                        , soil_heat_flux_fraction)
*====================================================================

      implicit none


*+  Sub-Program Arguments
      real       radn                    ! (INPUT) Incoming Radiation
      real       RadnInt                 ! (INPUT) Intercepted incoming radiation
      real       soil_heat_flux_fraction ! (INPUT) Fraction of surface radiation absorbed

*+  Purpose
*     calculate the daytime soil heat flux

*+  Changes
*

*+  Calls

*+  Local Variables


*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_Soil_Heat_Flux')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      micromet_Soil_Heat_Flux = - soil_heat_flux_fraction
     :                        * (Radn - RadnInt)

      micromet_soil_heat_flux = bound(micromet_soil_heat_flux
     :                               ,-Radn*0.1
     :                               ,0.0)

      call pop_routine (myname)
      return
      end function
