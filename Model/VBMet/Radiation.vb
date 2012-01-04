Imports System.Math
Public Module Radiation
    Public Const itns As Double = 10.0
    Public Const Taz As Double = 0.87  'Atmospheric Transmissivity at Azimuth - From Thornton and Running.
    Public Const Alpha As Double = 0.0061 ' Decrease in Atmospheric Transmissivity per unit water vapour - From Thornton and Running

    ' ------------------------------------------------------------------------
    Function day_length(ByVal day As Integer, ByVal lat As Double, ByVal sun_angle As Double) As Double
        ' ------------------------------------------------------------------------
        Dim DEC As Double      ' Declination - degrees
        Dim DECr As Double     ' Declination - radians
        Dim LATr As Double     ' Latitude - radians
        Dim HS As Double       ' Hour angle
        Dim sun_alt As Double  ' solar altitude
        Dim coshra As Double   ' Cosine of hour angle
        Dim slsd As Double
        Dim clcd As Double
        Dim altmn As Double
        Dim altmx As Double
        Dim alt As Double

        sun_alt = sun_angle * 2.0 * PI / 365.25

        DEC = 23.45 * Sin(2.0 * PI / 365.25 * (day - 79.25))
        DECr = DEC * 2.0 * PI / 360.0
        LATr = lat * 2.0 * PI / 360.0

        If lat = 90 Then
            coshra = Sign(-DEC) * Sign(lat)
        Else
            slsd = Sin(LATr) * Sin(DECr)
            clcd = Cos(LATr) * Cos(DECr)

            altmn = Asin(bound(slsd - clcd, -1.0, 1.0))
            altmx = Asin(bound(slsd + clcd, -1.0, 1.0))
            alt = bound(sun_alt, altmn, altmx)

            ' get cos of the hour angle

            coshra = (Sin(alt) - slsd) / clcd
            coshra = bound(coshra, -1.0, 1.0)
        End If

        HS = Acos(coshra)

        day_length = HS * 2.0 * 24.0 / (2.0 * PI)

    End Function

    Function TandR_radn(ByVal day As Integer, ByVal lat As Double, ByVal dT As Double, ByVal vp As Double, ByVal rain As Double, ByVal dT30 As Double) As Double
        Dim TransMax As Double
        Dim b As Double
        Dim c As Double
        Dim Tfmax As Double

        ' NOTE - this needs to be converted back to stardard published approach

        TransMax = TMax(day, lat, Taz, Alpha, vp)

        'b = 0.031 + 0.201 * Exp(-0.185 * dT30)
        b = 0.08

        c = 1.5

        'If rain > 0 Then
        '   dT = dT * 0.75
        'End If

        Tfmax = 1 - 0.9 * Exp(-b * dT ^ c)
        If rain > 25 Then
            Tfmax = Tfmax * 0.5
            'Tfmax = max(Tfmax, 0.5)
        End If

        TandR_radn = Q0(day, lat) * Tfmax * TransMax

    End Function


    Function Q0(ByVal day As Integer, ByVal lat As Double) As Double

        ' Total Daily Extraterrestrial SW radiation - MJ

        Dim DEC As Double
        Dim DECr As Double
        Dim LATr As Double
        Dim HS As Double


        DEC = 23.45 * Sin(2.0 * PI / 365.25 * (day - 79.25))
        DECr = DEC * 2.0 * PI / 360.0
        LATr = lat * 2.0 * PI / 360.0

        HS = Acos(-Tan(LATr) * Tan(DECr))
        Q0 = 86400.0 * 1360.0 * (HS * Sin(LATr) * Sin(DECr) + Cos(LATr) * Cos(DECr) * _
             Sin(HS)) / PI
        Q0 = Q0 / 1000000.0

    End Function

    ' ------------------------------------------------------------------------
    Function Transmissivity(ByVal day As Integer, ByVal lat As Double, ByVal Radn As Double) As Double
        ' ------------------------------------------------------------------------
        Transmissivity = Radn / Q0(day, lat)
    End Function

    ' ------------------------------------------------------------------------
    Function Q0i(ByVal day As Integer, ByVal HS As Double, ByVal lat As Double) As Double
        ' ------------------------------------------------------------------------

        ' Instantaneous Extraterrestrial SW radiation - MJ

        Dim DEC As Double
        Dim DECr As Double
        Dim LATr As Double
        Dim HSmax As Double

        HSmax = Acos(-Tan(LATr) * Tan(DECr)) ' half daylength
        If (HS <= HSmax) Then

            DEC = 23.45 * Sin(2.0 * PI / 365.25 * (day - 79.25))

            DECr = DEC * 2.0 * PI / 360.0
            LATr = lat * 2.0 * PI / 360.0

            Q0i = 1360.0 * (Sin(LATr) * Sin(DECr) + Cos(LATr) * Cos(DECr) * Cos(HS))
            Q0i = Q0i / 1000000.0 ' Convert to MJ
        Else
            Q0i = 0
        End If

    End Function

    ' ------------------------------------------------------------------------
    Function Q0int(ByVal day As Integer, ByVal lat As Double) As Double
        ' ------------------------------------------------------------------------
        ' Total Daily Extraterrestrial SW radiation - MJ
        ' Integrated from instantaneous values of S0

        Dim DEC As Double
        Dim DECr As Double
        Dim LATr As Double
        Dim T As Double 'time of day as hour angle in radians
        Dim S1, S2 As Double
        Dim HS As Double

        DEC = 23.45 * Sin(2.0 * PI / 365.25 * (day - 79.25))
        DECr = DEC * 2.0 * PI / 360.0
        LATr = lat * 2.0 * PI / 360.0

        HS = Acos(-Tan(LATr) * Tan(DECr)) ' half daylength

        ' Integrate using trapezoidal rule type thing
        Q0int = 0.0
        For T = HS / itns To HS Step HS / itns
            S1 = Q0i(day, T - HS / itns, lat)
            S2 = Q0i(day, T, lat)
            Q0int = Q0int + (S1 + S2) / 2 / itns
        Next T
        Q0int = Q0int * HS / (2.0 * PI / (24.0 * 60.0 * 60.0))
        Q0int = Q0int * 2.0


    End Function
    ' ------------------------------------------------------------------------
    Function QMax(ByVal day As Integer, ByVal lat As Double, ByVal Taz As Double, ByVal alpha As Double, ByVal vp As Double) As Double
        ' ------------------------------------------------------------------------
        Dim DEC As Double
        Dim DECr As Double
        Dim LATr As Double
        Dim T As Double 'time of day as hour angle in radians
        Dim S1, S2 As Double
        Dim HS As Double ' Sunrise hour angle
        Dim M As Double ' Mixing Length

        DEC = 23.45 * Sin(2.0 * PI / 365.25 * (day - 79.25))
        DECr = DEC * 2.0 * PI / 360.0
        LATr = lat * 2.0 * PI / 360.0

        HS = Acos(-Tan(LATr) * Tan(DECr)) ' half daylength

        ' Integrate using trapezoidal rule type thing
        QMax = 0.0
        For T = HS / itns To HS Step HS / itns
            'avoid very small angles causing numerical errors
            T = Min(T, HS * 0.999)
            S1 = Q0i(day, T - HS / itns, lat)
            S2 = Q0i(day, T, lat)
            ' note that sin(a) = cos(z) therefore
            M = 1 / (Sin(LATr) * Sin(DECr) + Cos(LATr) * Cos(DECr) * Cos(T))

            If M > 2 Then
                ' 1/cos(z) rule breaks down at low angles due to earth's curvature
                ' add correction fitted to Smithsonian met table data
                M = 2.0 + (M - 2.0) ^ 0.955
            End If
            QMax = QMax + (S1 + S2) / 2 / itns * ((Taz ^ M) - alpha * vp)
        Next T
        QMax = QMax * HS / (2.0 * PI / (24.0 * 60.0 * 60.0))
        QMax = QMax * 2.0

    End Function

    ' ------------------------------------------------------------------------
    Function TMax(ByVal day As Integer, ByVal lat As Double, ByVal Taz As Double, ByVal alpha As Double, ByVal vp As Double) As Double
        ' ------------------------------------------------------------------------
        Dim S, Q As Double
        S = QMax(day, lat, Taz, alpha, vp)
        Q = Q0(day, lat)
        TMax = S / Q

    End Function
    ' ------------------------------------------------------------------------
    Function M(ByVal day As Double, ByVal lat As Double, ByVal HS As Double) As Double
        ' ------------------------------------------------------------------------
        Dim DEC As Double
        Dim DECr As Double
        Dim LATr As Double
        Dim HSmax As Double

        DEC = 23.45 * Sin(2.0 * 3.14159265 / 365.25 * (day - 79.25))
        DECr = DEC * 2.0 * 3.14159265 / 360.0
        LATr = lat * 2.0 * 3.14159265 / 360.0

        HSmax = Acos(-Tan(LATr) * Tan(DECr)) ' half daylength
        HS = Min(HS, HSmax * 0.99)

        ' note that sin(a) = cos(z) therefore
        M = 1 / (Sin(LATr) * Sin(DECr) + Cos(LATr) * Cos(DECr) * Cos(HS))

        If M > 2 Then
            ' 1/cos(z) rule breaks down at low angles due to earth's curvature
            ' add correction fitted to Smithsonian met table data
            M = 2.0 + (M - 2.0) ^ 0.955
            M = Min(M, 20.0)
            M = Max(M, 0.0)

        End If

    End Function
    ' ------------------------------------------------------------------------
    Function Mint(ByVal day As Double, ByVal lat As Double, ByVal HS1 As Double, ByVal HS2 As Double) As Double
        ' ------------------------------------------------------------------------
        Dim dHS As Double
        Dim start As Double
        Dim finish As Double
        Dim T As Double
        Dim M1, M2 As Double
        Dim i As Integer

        dHS = Abs(HS2 - HS1)
        start = Min(HS1, HS2)
        finish = Max(HS1, HS2)

        Mint = 0.0

        For i = 1 To CInt(itns)

            T = start + i * dHS / itns
            M1 = M(day, lat, T - dHS / itns)
            M2 = M(day, lat, T)
            Mint = Mint + (M1 + M2) / 2 / itns
        Next
        'Beep

    End Function

    ' ------------------------------------------------------------------------
    Function Q0iint(ByVal day As Integer, ByVal lat As Double, ByVal HS1 As Double, ByVal HS2 As Double) As Double
        Dim T As Double
        Dim S1, S2 As Double
        Dim dHS As Double
        Dim start As Double
        Dim finish As Double

        Q0iint = 0.0
        dHS = Abs(HS2 - HS1)
        start = Min(HS1, HS2)
        finish = Max(HS1, HS2)

        For T = start + dHS / itns To finish Step dHS / itns
            S1 = Q0i(day, T - dHS / itns, lat)
            S2 = Q0i(day, T, lat)
            Q0iint = Q0iint + (S1 + S2) / 2 / itns
        Next

    End Function
    ' ------------------------------------------------------------------------
    Function bound(ByVal x As Double, ByVal x1 As Double, ByVal x2 As Double) As Double
        bound = Min(Max(x, x1), x2)
    End Function

End Module
