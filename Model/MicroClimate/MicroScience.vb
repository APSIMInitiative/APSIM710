Imports System
Imports System.Collections.Generic
Imports System.Text
Imports CSGeneral

Partial Public Class MicroClimate

    Private Function CalcDayLength(ByVal latitude As Double, ByVal day As Integer, ByVal sunAngle As Double) As Double
        Return MathUtility.DayLength(day, sunAngle, latitude)
    End Function

    Private Function CalcAverageT(ByVal mint As Double, ByVal maxt As Double) As Double
        Return 0.75 * maxt + 0.25 * mint
    End Function

    Private Function CalcSunshineHours(ByVal rand As Double, ByVal dayLengthLight As Double, ByVal latitude As Double, ByVal day As Double) As Double
        Dim maxSunHrs As Double = dayLengthLight

        Dim relativeDistance As Double = 1.0 + 0.033 * Math.Cos(0.0172 * day)

        Dim solarDeclination As Double = 0.409 * Math.Sin(0.0172 * day - 1.39)

        Dim sunsetAngle As Double = Math.Acos(-Math.Tan(latitude * Deg2Rad) * Math.Tan(solarDeclination))

        Dim extraTerrestrialRadn As Double = 37.6 * relativeDistance * _
                                         (sunsetAngle * Math.Sin(latitude * Deg2Rad) * _
                                          Math.Sin(solarDeclination) + Math.Cos(latitude * Deg2Rad) * _
                                          Math.Cos(solarDeclination) * Math.Sin(sunsetAngle))

        Dim maxRadn As Double = 0.75 * extraTerrestrialRadn

        ' finally calculate the sunshine hours as the ratio of
        ' maximum possible radiation
        Return Math.Min(maxSunHrs * radn / maxRadn, maxSunHrs)
    End Function

    ''' <summary>
    ''' Calculate the crop canopy conductance
    ''' <param name="cropGsMax">crop-specific maximum stomatal conductance (m/s)</param>
    ''' <param name="cropR50">crop-specific SolRad at which stomatal conductance decreases to 50% (W/m2)</param>
    ''' <param name="cropRGfac">crop-specific relative growth stress factor (0-1)</param>
    ''' <param name="cropLAIfac">crop-specific LAI fraction of total LAI in current layer (0-1)</param>
    ''' <param name="layerK">layer-averaged light extinction coeficient (-)</param>
    ''' <param name="layerLAI">LAI within the current layer (m2/m2)</param>
    ''' <param name="layerSolRad">solar radiation arriving at the top of the current layer(W/m2)</param>
    ''' </summary>
    Private Function CanopyConductance(ByVal cropGsMax As Double, ByVal cropR50 As Double, ByVal cropRGfac As Double, _
                                       ByVal cropLAIfac As Double, ByVal layerK As Double, ByVal layerLAI As Double, _
                                       ByVal layerSolRad As Double) As Double

        Dim numerator As Double = layerSolRad + cropR50
        Dim denominator As Double = layerSolRad * Math.Exp(-1.0 * layerK * layerLAI) + cropR50
        Dim hyperbolic As Double = MathUtility.Divide(numerator, denominator, 0.0)

        hyperbolic = Math.Max(1.0, hyperbolic)

        Return Math.Max(0.0001, MathUtility.Divide(cropGsMax * cropRGfac * cropLAIfac, layerK, 0.0) * Math.Log(hyperbolic))
    End Function

    ''' <summary>
    ''' Calculate the aerodynamic conductance using FAO approach
    ''' </summary>
    Private Function AerodynamicConductanceFAO(ByVal windSpeed As Double, ByVal refHeight As Double, _
                                               ByVal topHeight As Double, ByVal LAItot As Double) As Double
        Const vonKarman As Double = 0.41
        Dim mterm As Double = 0.0
        ' momentum term in Ga calculation
        Dim hterm As Double = 0.0
        ' heat term in Ga calculation
        ' Calculate site properties
        Dim d As Double = 0.666 * topHeight
        ' zero plane displacement height (m)
        Dim Zh As Double = topHeight + refHeight
        ' height of humidity measurement (m) - assume reference above canopy
        Dim Zm As Double = topHeight + refHeight
        ' height of wind measurement (m)
        Dim Z0m As Double = 0.123 * topHeight
        ' roughness length governing transfer of momentum (m)
        Dim Z0h As Double = 0.1 * Z0m
        ' roughness length governing transfer of heat and vapour (m)
        ' Calcuate conductance

        If (Z0m <> 0) AndAlso (Z0h <> 0) Then
            mterm = MathUtility.Divide(vonKarman, Math.Log(MathUtility.Divide(Zm - d, Z0m, 0.0)), 0.0)
            hterm = MathUtility.Divide(vonKarman, Math.Log(MathUtility.Divide(Zh - d, Z0h, 0.0)), 0.0)
        End If

        Return Math.Max(0.001, windSpeed * mterm * hterm)
    End Function

    ''' <summary>
    ''' Calculate the Penman-Monteith water demand
    ''' </summary>
    Private Function CalcPenmanMonteith(ByVal rn As Double, ByVal mint As Double, ByVal maxt As Double, ByVal vp As Double, _
                                        ByVal airPressure As Double, ByVal day_length As Double, _
                                        ByVal Ga As Double, ByVal Gc As Double) As Double
        Dim averageT As Double = CalcAverageT(mint, maxt)
        Dim nondQsdT As Double = CalcNondQsdT(averageT, airPressure)
        Dim RhoA As Double = CalcRhoA(averageT, airPressure)
        Dim lambda As Double = CalcLambda(averageT)

        Dim specificVPD As Double = CalcSpecificVPD(vp, mint, maxt, airPressure)
        Dim denominator As Double = nondQsdT + MathUtility.Divide(Ga, Gc, 0.0) + 1.0 ' unitless

        Dim PETr As Double = MathUtility.Divide(nondQsdT * rn, denominator, 0.0) * 1000.0 / lambda / RhoW

        Dim PETa As Double = MathUtility.Divide(RhoA * specificVPD * Ga, denominator, 0.0) * 1000.0 * (day_length * hr2s) / RhoW

        Return PETr + PETa
    End Function

    ''' <summary>
    ''' Calculate the radiation-driven term for the Penman-Monteith water demand
    ''' </summary>
    Private Function CalcPETr(ByVal rn As Double, ByVal mint As Double, ByVal maxt As Double, ByVal airPressure As Double, ByVal Ga As Double, ByVal Gc As Double) As Double
        Dim averageT As Double = CalcAverageT(mint, maxt)
        Dim nondQsdT As Double = CalcNondQsdT(averageT, airPressure)
        Dim lambda As Double = CalcLambda(averageT)
        Dim denominator As Double = nondQsdT + MathUtility.Divide(Ga, Gc, 0.0) + 1.0

        Return MathUtility.Divide(nondQsdT * rn, denominator, 0.0) * 1000.0 / lambda / RhoW

    End Function

    ''' <summary>
    ''' Calculate the aerodynamically-driven term for the Penman-Monteith water demand
    ''' </summary>
    Private Function CalcPETa(ByVal mint As Double, ByVal maxt As Double, ByVal vp As Double, ByVal airPressure As Double, ByVal day_length As Double, ByVal Ga As Double, _
     ByVal Gc As Double) As Double
        Dim averageT As Double = CalcAverageT(mint, maxt)
        Dim nondQsdT As Double = CalcNondQsdT(averageT, airPressure)
        Dim lambda As Double = CalcLambda(averageT)
        Dim denominator As Double = nondQsdT + MathUtility.Divide(Ga, Gc, 0.0) + 1.0

        Dim RhoA As Double = CalcRhoA(averageT, airPressure)

        Dim specificVPD As Double = CalcSpecificVPD(vp, mint, maxt, airPressure)

        Return MathUtility.Divide(RhoA * specificVPD * Ga, denominator, 0.0) * 1000.0 * (day_length * hr2s) / RhoW

    End Function

    ''' <summary>
    ''' Calculate the density of air (kg/m3) at a given temperature
    ''' </summary>
    Private Function CalcRhoA(ByVal temperature As Double, ByVal airPressure As Double) As Double
        ' air pressure converted to Pa
        Return MathUtility.Divide(mwair * airPressure * 100.0, (abs_temp + temperature) * r_gas, 0.0)
    End Function

    ''' <summary>
    ''' Calculate the Jarvis &amp; McNaughton decoupling coefficient, omega
    ''' </summary>
    Private Function CalcOmega(ByVal mint As Double, ByVal maxt As Double, ByVal airPressure As Double, _
                               ByVal aerodynamicCond As Double, ByVal canopyCond As Double) As Double
        Dim Non_dQs_dT As Double = CalcNondQsdT((mint + maxt) / 2.0, airPressure)
        Return MathUtility.Divide(Non_dQs_dT + 1.0, Non_dQs_dT + 1.0 + MathUtility.Divide(aerodynamicCond, canopyCond, 0.0), 0.0)
    End Function

    ''' <summary>
    ''' Calculate Non_dQs_dT - the dimensionless valu for 
    ''' d(sat spec humidity)/dT ((kg/kg)/K) FROM TETEN FORMULA
    ''' </summary>
    Private Function CalcNondQsdT(ByVal temperature As Double, ByVal airPressure As Double) As Double
        Dim esat As Double = CalcSVP(temperature)
        ' saturated vapour pressure (mb)
        Dim desdt As Double = esat * svp_B * svp_C / Math.Pow(svp_C + temperature, 2.0)
        ' d(sat VP)/dT : (mb/K)
        Dim dqsdt As Double = (mwh2o / mwair) * desdt / airPressure
        ' d(sat spec hum)/dT : (kg/kg)/K
        Return CalcLambda(temperature) / Cp * dqsdt
    End Function

    ''' <summary>
    ''' Calculate the saturated vapour pressure for a given temperature
    ''' </summary>
    Private Function CalcSVP(ByVal temperature As Double) As Double
        Return svp_A * Math.Exp(svp_B * temperature / (temperature + svp_C))
    End Function

    ''' <summary>
    ''' Calculate the vapour pressure deficit
    ''' <param name="vp">(INPUT) vapour pressure (hPa = mbar)</param>
    ''' <param name="mint">(INPUT) minimum temperature (oC)</param>
    ''' <param name="maxt">(INPUT) maximum temperature (oC)</param>
    ''' <param name="airPressure">(INPUT) Air pressure (hPa)</param>
    ''' </summary>
    Private Function CalcSpecificVPD(ByVal vp As Double, ByVal mint As Double, ByVal maxt As Double, ByVal airPressure As Double) As Double
        Dim VPD As Double = CalcVPD(vp, mint, maxt)
        Return CalcSpecificHumidity(VPD, airPressure)
    End Function

    ''' <summary>
    ''' Calculate specific humidity from vapour pressure
    ''' <param name="vp">vapour pressure (hPa = mbar)</param>
    ''' <param name="airPressure">air pressure (hPa)</param>
    ''' </summary>
    Private Function CalcSpecificHumidity(ByVal vp As Double, ByVal airPressure As Double) As Double
        Return (mwh2o / mwair) * vp / airPressure
    End Function

    ''' <summary>
    ''' Calculate the vapour pressure deficit
    ''' <param name="vp">(INPUT) vapour pressure (hPa = mbar)</param>
    ''' <param name="mint">(INPUT) minimum temperature (oC)</param>
    ''' <param name="maxt">(INPUT) maximum temperature (oC)</param>
    ''' </summary>
    Private Function CalcVPD(ByVal vp As Double, ByVal mint As Double, ByVal maxt As Double) As Double
        Dim VPDmint As Double = Math.Max(0.0, CalcSVP(mint) - vp)
        ' VPD at minimum temperature
        Dim VPDmaxt As Double = Math.Max(0.0, CalcSVP(maxt) - vp)
        ' VPD at maximum temperature
        Return svp_fract * VPDmaxt + (1 - svp_fract) * VPDmint
    End Function

    ''' <summary>
    ''' Calculate the lambda (latent heat of vapourisation for water) (J/kg)
    ''' </summary>
    Private Function CalcLambda(ByVal temperature As Double) As Double
        Return (2501.0 - 2.38 * temperature) * 1000.0
        ' J/kg
    End Function
    ''' <summary>
    ''' Calculates interception of short wave by canopy compartments
    ''' </summary>
    Private Sub ShortWaveRadiation()
        ' Perform Top-Down Light Balance
        ' ==============================
        Dim Rin As Double = radn
        Dim Rint As Double
        For i As Integer = numLayers - 1 To 0 Step -1
            Rint = Rin * (1.0 - Math.Exp(-layerKtot(i) * layerLAIsum(i)))

            For j As Integer = 0 To ComponentData.Length - 1
                ComponentData(j).Rs(i) = Rint * MathUtility.Divide(ComponentData(j).Ftot(i) * ComponentData(j).Ktot, layerKtot(i), 0.0)
            Next
            Rin -= Rint
        Next
    End Sub

    ''' <summary>
    ''' Calculate the overall system energy terms
    ''' </summary>
    Private Sub EnergyTerms()
        sumRs = 0.0
        albedo = 0.0
        emissivity = 0.0

        For i As Integer = numLayers - 1 To 0 Step -1
            For j As Integer = 0 To ComponentData.Length - 1
                albedo += MathUtility.Divide(ComponentData(j).Rs(i), radn, 0.0) * ComponentData(j).Albedo
                emissivity += MathUtility.Divide(ComponentData(j).Rs(i), radn, 0.0) * ComponentData(j).Emissivity
                sumRs += ComponentData(j).Rs(i)
            Next
        Next

        albedo += (1.0 - MathUtility.Divide(sumRs, radn, 0.0)) * soil_albedo
        emissivity += (1.0 - MathUtility.Divide(sumRs, radn, 0.0)) * soil_emissivity
    End Sub

    ''' <summary>
    ''' Calculate Net Long Wave Radiation Balance
    ''' </summary>
    Private Sub LongWaveRadiation()
        netLongWave = LongWave(averageT, fractionClearSky, emissivity) * dayLength * hr2s / 1000000.0
        ' W to MJ
        ' Long Wave Balance Proportional to Short Wave Balance
        ' ====================================================
        For i As Integer = numLayers - 1 To 0 Step -1
            For j As Integer = 0 To ComponentData.Length - 1
                ComponentData(j).Rl(i) = MathUtility.Divide(ComponentData(j).Rs(i), radn, 0.0) * netLongWave
            Next
        Next
    End Sub

    ''' <summary>
    ''' Calculate the net longwave radiation 'in' (W/m2)
    ''' <param name="temperature">temperature  (oC)</param>
    ''' <param name="fracClearSkyRad">R/Ro, SunshineHrs/DayLength (0-1)</param>
    ''' <param name="emmisCanopy">canopy emmissivity</param>
    ''' <returns>net longwave radiation 'in' (W/m2)</returns>
    ''' </summary>
    Private Function LongWave(ByVal temperature As Double, ByVal fracClearSkyRad As Double, ByVal emmisCanopy As Double) As Double
        '  Notes
        '   Emissivity of the sky comes from Swinbank, W.C. (1963).
        '   Longwave radiation from clear skies Quart. J. Roy. Meteorol.
        '   Soc. 89, 339-348.

        '  Changes
        '      291098 - NIH Adapted from Grandis module
        '       050799 - VOS Changed sign so that is net INWARDS longwave
        '       060799 - VOS Changed arguments from sunhine hours and daylength
        '          to FracClearSkyRadn for compatability with variable timestep

        Dim emmisSky As Double
        ' emmisivity of the sky
        Dim cloudEffect As Double
        ' cloud effect on net long wave (0-1)
        emmisSky = 0.00000937 * Math.Pow(temperature + abs_temp, 2.0)

        ' assume constant value for now
        ' emmisSky = 0.80

        fracClearSkyRad = Math.Max(0.0, Math.Min(1.0, fracClearSkyRad))
        cloudEffect = (c_cloud + (1.0 - c_cloud) * fracClearSkyRad)

        ' remove cloud effect for now
        ' cloud_effect = 1.0

        Return cloudEffect * (emmisSky - emmisCanopy) * stef_boltz * Math.Pow(temperature + abs_temp, 4.0)

        ' Try Monteith approach
        ' return -(107. - 0.3 * temperature);
    End Function

    ''' <summary>
    ''' Calculate Radiation loss to soil heating
    ''' </summary>
    Private Sub SoilHeatRadiation()
        Dim radnint As Double
        ' Intercepted SW radiation
        radnint = sumRs
        soil_heat = SoilHeatFlux(radn, radnint, soil_heat_flux_fraction)

        ' soil_heat = -0.1 * ((1.0 - albedo) * radn * netLongWave;

        ' SoilHeat balance Proportional to Short Wave Balance
        ' ====================================================
        For i As Integer = numLayers - 1 To 0 Step -1
            For j As Integer = 0 To ComponentData.Length - 1
                ComponentData(j).Rsoil(i) = MathUtility.Divide(ComponentData(j).Rs(i), radn, 0.0) * soil_heat
            Next
        Next
    End Sub

    ''' <summary>
    ''' Calculate the daytime soil heat flux
    ''' <param name="radn">(INPUT) Incoming Radiation</param>
    ''' <param name="radnint">(INPUT) Intercepted incoming radiation</param>
    ''' <param name="soilHeatFluxFraction">(INPUT) Fraction of surface radiation absorbed</param>
    ''' </summary>
    Private Function SoilHeatFlux(ByVal radn As Double, ByVal radnint As Double, ByVal soilHeatFluxFraction As Double) As Double
        Return Math.Max(-radn * 0.1, Math.Min(0.0, -soilHeatFluxFraction * (radn - radnint)))
    End Function

    ''' <summary>
    ''' Calculate the proportion of light intercepted by a given component that corresponds to green leaf
    ''' </summary>
    Private Function RadnGreenFraction(ByVal j As Integer) As Double
        Dim klGreen As Double = -Math.Log(1.0 - ComponentData(j).CoverGreen)
        Dim klTot As Double = -Math.Log(1.0 - ComponentData(j).CoverTot)
        Return MathUtility.Divide(klGreen, klTot, 0.0)
    End Function

End Class
