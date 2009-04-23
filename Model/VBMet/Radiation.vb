Imports System.Math
Public Module Radiation
    Public Const itns As Single = 10.0#
    Public Const Taz As Single = 0.87  'Atmospheric Transmissivity at Azimuth - From Thornton and Running.
    Public Const Alpha As Single = 0.0061 ' Decrease in Atmospheric Transmissivity per unit water vapour - From Thornton and Running

    ' ------------------------------------------------------------------------
    Function day_length(ByVal day As Integer, ByVal lat As Single, ByVal sun_angle As Single) As Single
        ' ------------------------------------------------------------------------
        Dim DEC As Single      ' Declination - degrees
        Dim DECr As Single     ' Declination - radians
        Dim LATr As Single     ' Latitude - radians
        Dim HS As Single       ' Hour angle
        Dim sun_alt As Single  ' solar altitude
        Dim coshra As Single   ' Cosine of hour angle
        Dim slsd As Single
        Dim clcd As Single
        Dim altmn As Single
        Dim altmx As Single
        Dim alt As Single

        sun_alt = sun_angle * 2.0# * PI / 365.25

        DEC = 23.45 * Sin(2.0# * PI / 365.25 * (day - 82.25))
        DECr = DEC * 2.0# * PI / 360.0#
        LATr = lat * 2.0# * PI / 360.0#

        If lat = 90 Then
            coshra = Sign(-DEC) * Sign(lat)
        Else
            slsd = Sin(LATr) * Sin(DECr)
            clcd = Cos(LATr) * Cos(DECr)

            altmn = asin(bound(slsd - clcd, -1.0#, 1.0#))
            altmx = asin(bound(slsd + clcd, -1.0#, 1.0#))
            alt = bound(sun_alt, altmn, altmx)

            ' get cos of the hour angle

            coshra = (Sin(alt) - slsd) / clcd
            coshra = bound(coshra, -1.0#, 1.0#)
        End If

        HS = acos(coshra)

        day_length = HS * 2.0# * 24.0# / (2.0# * PI)

    End Function

    Function TandR_radn(ByVal day As Integer, ByVal lat As Single, ByVal dT As Single, ByVal vp As Single, ByVal rain As Single, ByVal dT30 As Single) As Single
        Dim TransMax As Single
        Dim b As Single
        Dim c As Single
        Dim Tfmax As Single

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


    Function Q0(ByVal day As Integer, ByVal lat As Single) As Single

        ' Total Daily Extraterrestrial SW radiation - MJ

        Dim DEC As Single
        Dim DECr As Single
        Dim LATr As Single
        Dim HS As Single


        DEC = 23.45 * Sin(2.0# * PI / 365.25 * (day - 82.25))
        DECr = DEC * 2.0# * PI / 360.0#
        LATr = lat * 2.0# * PI / 360.0#

        HS = acos(-Tan(LATr) * Tan(DECr))
        Q0 = 86400.0# * 1360.0# * (HS * Sin(LATr) * Sin(DECr) + Cos(LATr) * Cos(DECr) * _
             Sin(HS)) / PI
        Q0 = Q0 / 1000000.0#

    End Function

    ' ------------------------------------------------------------------------
    Function Transmissivity(ByVal day As Integer, ByVal lat As Single, ByVal Radn As Single) As Single
        ' ------------------------------------------------------------------------
        Transmissivity = Radn / Q0(day, lat)
    End Function

    ' ------------------------------------------------------------------------
    Function Q0i(ByVal day As Integer, ByVal HS As Single, ByVal lat As Single) As Single
        ' ------------------------------------------------------------------------

        ' Instantaneous Extraterrestrial SW radiation - MJ

        Dim DEC As Single
        Dim DECr As Single
        Dim LATr As Single
        Dim HSmax As Single

        HSmax = acos(-Tan(LATr) * Tan(DECr)) ' half daylength
        If (HS <= HSmax) Then

            DEC = 23.45 * Sin(2.0# * PI / 365.25 * (day - 82.25))

            DECr = DEC * 2.0# * PI / 360.0#
            LATr = lat * 2.0# * PI / 360.0#

            Q0i = 1360.0# * (Sin(LATr) * Sin(DECr) + Cos(LATr) * Cos(DECr) * Cos(HS))
            Q0i = Q0i / 1000000.0# ' Convert to MJ
        Else
            Q0i = 0
        End If

    End Function

    ' ------------------------------------------------------------------------
    Function Q0int(ByVal day As Integer, ByVal lat As Single) As Single
        ' ------------------------------------------------------------------------
        ' Total Daily Extraterrestrial SW radiation - MJ
        ' Integrated from instantaneous values of S0

        Dim DEC As Single
        Dim DECr As Single
        Dim LATr As Single
        Dim T As Single 'time of day as hour angle in radians
        Dim S1, S2 As Single
        Dim HS As Single

        DEC = 23.45 * Sin(2.0# * PI / 365.25 * (day - 82.25))
        DECr = DEC * 2.0# * PI / 360.0#
        LATr = lat * 2.0# * PI / 360.0#

        HS = acos(-Tan(LATr) * Tan(DECr)) ' half daylength

        ' Integrate using trapezoidal rule type thing
        Q0int = 0.0#
        For T = HS / itns To HS Step HS / itns
            S1 = Q0i(day, T - HS / itns, lat)
            S2 = Q0i(day, T, lat)
            Q0int = Q0int + (S1 + S2) / 2 / itns
        Next T
        Q0int = Q0int * HS / (2.0# * PI / (24.0# * 60.0# * 60.0#))
        Q0int = Q0int * 2.0#


    End Function
    ' ------------------------------------------------------------------------
    Function QMax(ByVal day As Integer, ByVal lat As Single, ByVal Taz As Single, ByVal alpha As Single, ByVal vp As Single) As Single
        ' ------------------------------------------------------------------------
        Dim DEC As Single
        Dim DECr As Single
        Dim LATr As Single
        Dim T As Single 'time of day as hour angle in radians
        Dim S1, S2 As Single
        Dim HS As Single ' Sunrise hour angle
        Dim M As Single ' Mixing Length

        DEC = 23.45 * Sin(2.0# * PI / 365.25 * (day - 82.25))
        DECr = DEC * 2.0# * PI / 360.0#
        LATr = lat * 2.0# * PI / 360.0#

        HS = acos(-Tan(LATr) * Tan(DECr)) ' half daylength

        ' Integrate using trapezoidal rule type thing
        QMax = 0.0#
        For T = HS / itns To HS Step HS / itns
            'avoid very small angles causing numerical errors
            T = min(T, HS * 0.999)
            S1 = Q0i(day, T - HS / itns, lat)
            S2 = Q0i(day, T, lat)
            ' note that sin(a) = cos(z) therefore
            M = 1 / (Sin(LATr) * Sin(DECr) + Cos(LATr) * Cos(DECr) * Cos(T))

            If M > 2 Then
                ' 1/cos(z) rule breaks down at low angles due to earth's curvature
                ' add correction fitted to Smithsonian met table data
                M = 2.0# + (M - 2.0#) ^ 0.955
            End If
            QMax = QMax + (S1 + S2) / 2 / itns * ((Taz ^ M) - alpha * vp)
        Next T
        QMax = QMax * HS / (2.0# * PI / (24.0# * 60.0# * 60.0#))
        QMax = QMax * 2.0#

    End Function

    ' ------------------------------------------------------------------------
    Function TMax(ByVal day As Integer, ByVal lat As Single, ByVal Taz As Single, ByVal alpha As Single, ByVal vp As Single) As Single
        ' ------------------------------------------------------------------------
        Dim S, Q As Single
        S = QMax(day, lat, Taz, alpha, vp)
        Q = Q0(day, lat)
        TMax = S / Q

    End Function
    ' ------------------------------------------------------------------------
    Function M(ByVal day As Single, ByVal lat As Single, ByVal HS As Single) As Single
        ' ------------------------------------------------------------------------
        Dim DEC As Single
        Dim DECr As Single
        Dim LATr As Single
        Dim HSmax As Single

        DEC = 23.45 * Sin(2.0# * 3.14159265 / 365.25 * (day - 82.25))
        DECr = DEC * 2.0# * 3.14159265 / 360.0#
        LATr = lat * 2.0# * 3.14159265 / 360.0#

        HSmax = acos(-Tan(LATr) * Tan(DECr)) ' half daylength
        HS = min(HS, HSmax * 0.99)

        ' note that sin(a) = cos(z) therefore
        M = 1 / (Sin(LATr) * Sin(DECr) + Cos(LATr) * Cos(DECr) * Cos(HS))

        If M > 2 Then
            ' 1/cos(z) rule breaks down at low angles due to earth's curvature
            ' add correction fitted to Smithsonian met table data
            M = 2.0# + (M - 2.0#) ^ 0.955
            M = min(M, 20.0#)
            M = max(M, 0.0#)

        End If

    End Function
    ' ------------------------------------------------------------------------
    Function Mint(ByVal day As Single, ByVal lat As Single, ByVal HS1 As Single, ByVal HS2 As Single) As Single
        ' ------------------------------------------------------------------------
        Dim dHS As Double
        Dim start As Double
        Dim finish As Double
        Dim T As Double
        Dim M1, M2 As Single
        Dim i As Integer

        dHS = Abs(HS2 - HS1)
        start = min(HS1, HS2)
        finish = max(HS1, HS2)

        Mint = 0.0#

        For i = 1 To itns

            T = start + i * dHS / itns
            M1 = M(day, lat, T - dHS / itns)
            M2 = M(day, lat, T)
            Mint = Mint + (M1 + M2) / 2 / itns
        Next
        'Beep

    End Function

    ' ------------------------------------------------------------------------
    Function Q0iint(ByVal day As Integer, ByVal lat As Single, ByVal HS1 As Single, ByVal HS2 As Single) As Single
        Dim T As Single
        Dim S1, S2 As Single
        Dim dHS As Single
        Dim start As Single
        Dim finish As Single

        Q0iint = 0.0#
        dHS = Abs(HS2 - HS1)
        start = min(HS1, HS2)
        finish = max(HS1, HS2)

        For T = start + dHS / itns To finish Step dHS / itns
            S1 = Q0i(day, T - dHS / itns, lat)
            S2 = Q0i(day, T, lat)
            Q0iint = Q0iint + (S1 + S2) / 2 / itns
        Next

    End Function
    ' ------------------------------------------------------------------------

    ' ------------------------------------------------------------------------
    Function max(ByVal x As Single, ByVal y As Single) As Single
        ' ------------------------------------------------------------------------
        If x > y Then
            max = x
        Else
            max = y
        End If
    End Function
    ' ------------------------------------------------------------------------
    Function min(ByVal x As Single, ByVal y As Single) As Single
        ' ------------------------------------------------------------------------
        If x > y Then
            min = y
        Else
            min = x
        End If
    End Function

    ' ------------------------------------------------------------------------
    Function asin(ByVal x As Single) As Single
        ' ------------------------------------------------------------------------
        asin = Atan(x / Sqrt(-x * x + 1.0#))
    End Function
    ' ------------------------------------------------------------------------
    Function acos(ByVal x As Single) As Single
        ' ------------------------------------------------------------------------
        acos = Atan(-x / Sqrt(-x * x + 1.0#)) + 2.0# * Atan(1.0#)
    End Function


    Function bound(ByVal x As Single, ByVal x1 As Single, ByVal x2 As Single) As Single
        bound = min(max(x, x1), x2)
    End Function

End Module
