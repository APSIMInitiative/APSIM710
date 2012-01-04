Imports System.Math
Public Module Humidity

    Public Const P As Double = 101000.0 'atmospheric pressure (Pa)
    Public Const Ma As Double = 28.966   'molecular weight of dry air (g/mole)
    Public Const Cp As Double = 1.01     'specific heat of air (J/g/K)
    Public Const R As Double = 8.3143    'Gas constand (J/mole/K)
    Public Const L As Double = 2440      'Latent heat of vaporisation (J/g)

    Function RelHum(ByVal Ta As Double, ByVal Tw As Double) As Double
        ' Relative Humidity - unitless
        ' Ta is air temperature (oC)
        ' Tw is wet bulb temperature (oC)

        Dim RhoV_t As Double
        Dim RhoVs_t As Double
        RhoV_t = RhoVs(Tw) - gamma(Ta) * (Ta - Tw)
        RhoVs_t = RhoVs(Ta)
        RelHum = RhoV_t / RhoVs_t
    End Function

    Function RhoV(ByVal Ta As Double, ByVal Tw As Double) As Double
        ' Vapour Density
        RhoV = RhoVs(Tw) - gamma(Ta) * (Ta - Tw)
    End Function

    Function RhoVs(ByVal T As Double) As Double
        ' Saturated Vapour Density
        Dim Tk As Double
        Tk = T + 273.3
        RhoVs = 217 * (Exp(54.87819 - (6790.4985 / Tk + 5.02808 * Log(Tk)))) / Tk
    End Function

    Function gamma(ByVal T As Double) As Double
        Dim Tk As Double ' Absolute Temperature in Kelvin
        Tk = T + 273.3
        gamma = P * Ma * Cp / (R * Tk * L)
    End Function

    Public Function svp(ByVal temp_c As Double) As Double
        'Saturation Vapour Pressure
        svp = 6.1078 * Exp(17.269 * temp_c / (237.3 + temp_c))
    End Function
End Module
