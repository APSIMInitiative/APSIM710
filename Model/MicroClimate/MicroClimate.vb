Imports System
Imports System.Collections.Generic
Imports System.Text
Imports CSGeneral

''' <remarks>
''' <para>
''' I have generally followed the original division of interface code and "science" code
''' into different units (formerly MicroMet.for and MicroScience.for)
''' </para>
'''
''' <para>
''' Function routines were changed slightly as part of the conversion process. The "micromet_"
''' prefixes were dropped, as the functions are now members of a MicroMet class, and that class
''' membership keeps them distinguishable from other functions. A prefix of "Calc" was added to
''' some function names (e.g., CalcAverageT) if, after dropping the old "micromet_" prefix, there
''' was potential confusion between the function name and a variable name.
''' </para>
'''
''' <para> The following Fortran routines, originally in MicroScience.for, were NOT converted, 
''' because they were not actively being used:</para>
''' <para>    micromet_PenmanMonteith (the converted routine below was originally micromet_Penman_Monteith from MicroMet.for)</para>
''' <para>    micromet_ActualCanopyCond (the routine below was originally micromet_CanopyConductance)</para>
''' <para>    micromet_FrictionVelocity</para>
''' <para>    micromet_ZeroPlaneDispl</para>
''' <para>    micromet_RoughnessLength</para>
''' <para>    micromet_Radn2SolRad</para>
''' <para>    micromet_FreeEvapRate</para>
''' <para>    micromet_AerodynamicConductance (the routine below was originally micromet_AerodynamicConductanceFAO)</para>
''' </remarks>
''' <summary>
''' A more-or-less direct port of the Fortran MicroMet model
''' Ported by Eric Zurcher Jun 2011, first to C#, then automatically
''' to VB via the converter in SharpDevelop.
''' </summary>
Partial Public Class MicroMet
    Inherits Instance
#Region "Parameters used to initialise the model"
#Region "Parameters set in the GUI by the user"
    <Param([Optional]:=True, MinVal:=0.0, MaxVal:=10.0)> _
    <Units("mm/mm")> _
    <Description("")> _
    Private a_interception As Double = 0.0

    <Param([Optional]:=True, MinVal:=0.0, MaxVal:=5.0)> _
    <Units("")> _
    <Description("")> _
    Private b_interception As Double = 1.0

    <Param([Optional]:=True, MinVal:=0.0, MaxVal:=10.0)> _
    <Units("mm")> _
    <Description("")> _
    Private c_interception As Double = 0.0

    <Param([Optional]:=True, MinVal:=0.0, MaxVal:=20.0)> _
    <Units("mm")> _
    <Description("")> _
    Private d_interception As Double = 0.0

#End Region

#Region "Parameters not normally settable from the GUI by the user"
    <Param(MinVal:=900.0, MaxVal:=1100.0)> _
    <Units("hPa")> _
    <Description("")> _
    Private air_pressure As Double = 1010

    <Param(MinVal:=0.9, MaxVal:=1.0)> _
    <Units("")> _
    <Description("")> _
    Private soil_emissivity As Double = 0.96

    <Param(MinVal:=0.0, MaxVal:=1.0)> _
    <Units("")> _
    <Description("")> _
    Private soil_albedo As Double = 0.3

    <Param(MinVal:=-20.0, MaxVal:=20.0)> _
    <Units("deg")> _
    <Description("")> _
    Private sun_angle As Double = 15.0

    <Param(MinVal:=0.0, MaxVal:=1.0)> _
    <Units("")> _
    <Description("")> _
    Private soil_heat_flux_fraction As Double = 0.4

    <Param(MinVal:=0.0, MaxVal:=1.0)> _
    <Units("")> _
    <Description("")> _
    Private night_interception_fraction As Double = 0.5

    <Param(MinVal:=0.0, MaxVal:=10.0)> _
    <Units("m/s")> _
    <Description("")> _
    Private windspeed_default As Double = 3.0

    <Param(MinVal:=0.0, MaxVal:=10.0)> _
    <Units("m")> _
    <Description("")> _
    Private refheight As Double = 2.0

    <Param(MinVal:=0.0, MaxVal:=1.0)> _
    <Units("0-1")> _
    <Description("")> _
    Private albedo As Double = 0.15

    <Param(MinVal:=0.9, MaxVal:=1.0)> _
    <Units("0-1")> _
    <Description("")> _
    Private emissivity As Double = 0.96

    <Param(MinVal:=0.0, MaxVal:=1.0)> _
    <Units("m/s")> _
    <Description("")> _
    Private gsmax As Double = 0.01

    <Param(MinVal:=0.0, MaxVal:=1000.0)> _
    <Units("W/m^2")> _
    <Description("")> _
    Private r50 As Double = 200

#End Region

#End Region

#Region "Drivers we obtain from other components"

    ' We don't really need latitude as a "driver"
    ' since only need to fetch the value during init2.
    ' It shouldn't change during the course of the simulation.
    ' [Input]
    ' [Units("deg")]
    ''' private double latitude;

    <Input([Optional]:=True)> _
    <Units("m/s")> _
    Private windspeed As Double = 0.0

#End Region

#Region "Outputs we make available"

    <Output()> _
    <Units("mm")> _
    <Description("interception")> _
    Private ReadOnly Property interception() As Double
        Get
            Dim totalInterception As Double = 0.0
            For i As Integer = 0 To numLayers - 1
                For j As Integer = 0 To ComponentData.Length - 1
                    totalInterception += ComponentData(j).interception(i)
                Next
            Next
            Return totalInterception
        End Get
    End Property

    <Output()> _
    <Units("")> _
    <Description("")> _
    Private ReadOnly Property gc() As Double
        ' Should this be returning a sum or an array instead of just the first value???
        Get
            Return If(((ComponentData.Length > 0) AndAlso (numLayers > 0)), ComponentData(0).Gc(0), 0.0)
        End Get
    End Property

    <Output()> _
    <Units("")> _
    <Description("")> _
    Private ReadOnly Property ga() As Double
        ' Should this be returning a sum or an array instead of just the first value???
        Get
            Return If(((ComponentData.Length > 0) AndAlso (numLayers > 0)), ComponentData(0).Ga(0), 0.0)
        End Get
    End Property

    <Output()> _
    <Units("")> _
    <Description("")> _
    Private ReadOnly Property petr() As Double
        Get
            Dim totalPetr As Double = 0.0
            For i As Integer = 0 To numLayers - 1
                For j As Integer = 0 To ComponentData.Length - 1
                    totalPetr += ComponentData(j).PETr(i)
                Next
            Next
            Return totalPetr
        End Get
    End Property

    <Output()> _
    <Units("")> _
    <Description("")> _
    Private ReadOnly Property peta() As Double
        Get
            Dim totalPeta As Double = 0.0
            For i As Integer = 0 To numLayers - 1
                For j As Integer = 0 To ComponentData.Length - 1
                    totalPeta += ComponentData(j).PETa(i)
                Next
            Next
            Return totalPeta
        End Get
    End Property

    <Output()> _
    <Units("")> _
    <Description("")> _
    Private ReadOnly Property net_radn() As Double
        Get
            Return radn * (1.0 - albedo) + netLongWave
        End Get
    End Property

    <Output()> _
    <Units("")> _
    <Description("")> _
    Private ReadOnly Property net_rs() As Double
        Get
            Return radn * (1.0 - albedo)
        End Get
    End Property

    <Output()> _
    <Units("")> _
    <Description("")> _
    Private ReadOnly Property net_rl() As Double
        Get
            Return netLongWave
        End Get
    End Property

    <Output()> _
    <Units("")> _
    <Description("")> _
    Private soil_heat As Double = 0.0

    <Output()> _
    <Units("")> _
    <Description("")> _
    Private dryleaffraction As Double = 0.0

    <Output()> _
    <Description("")> _
    Private ReadOnly Property gsmax_array As KeyValueArrayType
        Get
            Dim _gsmax_array As New KeyValueArrayType
            Array.Resize(Of KeyValueArraypair_listType)(_gsmax_array.pair_list, ComponentData.Length)
            For j As Integer = 0 To ComponentData.Length - 1
                _gsmax_array.pair_list(j) = New KeyValueArraypair_listType
                _gsmax_array.pair_list(j).key = ComponentData(j).Name
                _gsmax_array.pair_list(j).value = ComponentData(j).Gsmax
            Next
            Return _gsmax_array
        End Get
    End Property
#End Region

#Region "Events which we publish"
    <[Event]()> _
    Public Event Canopy_Water_Balance As CanopyWaterBalanceDelegate

    <[Event]()> _
    Public Event Canopy_Energy_Balance As CanopyEnergyBalanceDelegate
#End Region

#Region "Events to which we subscribe, and their handlers"
    <EventHandler()> _
    Public Sub OnLai_Table()
        WriteTable("LAI")
    End Sub

    <EventHandler()> _
    Public Sub OnFtot_Table()
        WriteTable("Ftot")
    End Sub

    <EventHandler()> _
    Public Sub OnFgreen_Table()
        WriteTable("Fgreen")
    End Sub

    <EventHandler()> _
    Public Sub OnRs_Table()
        WriteTable("Rs")
    End Sub

    <EventHandler()> _
    Public Sub OnRl_Table()
        WriteTable("Rl")
    End Sub

    <EventHandler()> _
    Public Sub OnGc_Table()
        WriteTable("Gc")
    End Sub

    <EventHandler()> _
    Public Sub OnGa_Table()
        WriteTable("Ga")
    End Sub

    <EventHandler()> _
    Public Sub OnPet_Table()
        WriteTable("PET")
    End Sub

    <EventHandler()> _
    Public Sub OnPetr_Table()
        WriteTable("PETr")
    End Sub

    <EventHandler()> _
    Public Sub OnPeta_Table()
        WriteTable("PETa")
    End Sub

    <EventHandler()> _
    Public Sub OnOmega_Table()
        WriteTable("Omega")
    End Sub

    <EventHandler()> _
    Public Sub OnTick(time As TimeType)
        todayHeaderWritten = False
        DateUtility.JulianDayNumberToDayOfYear(time.startday, day, year)
    End Sub

    <EventHandler()> _
    Public Sub OnChangeGSMax(ChangeGSMax As ChangeGSMaxType)
        Dim senderIdx As Integer = FindComponentIndex(ChangeGSMax.component)
        If senderIdx < 0 Then
            Throw New Exception("Unknown Canopy Component: " & Convert.ToString(ChangeGSMax.component))
        End If
        ComponentData(senderIdx).Gsmax += ChangeGSMax.dlt
    End Sub



    ''' <summary>
    ''' Obtain all relevant met data
    ''' </summary>
    <EventHandler()> _
    Public Sub OnNewmet(NewMet As NewMetType)
        radn = NewMet.radn
        maxt = NewMet.maxt
        mint = NewMet.mint
        rain = NewMet.rain
        vp = NewMet.vp
    End Sub

    <EventHandler()> _
    Public Sub OnProcess()
        CalculateGc()
        CalculateGa()
        CalculateInterception()
        CalculatePM()
        CalculateOmega()

        SendEnergyBalanceEvent()
        SendWaterBalanceEvent()
    End Sub

    <EventHandler()> _
    Public Sub OnPrepare()
        MetVariables()
        CanopyCompartments()
        BalanceCanopyEnergy()
    End Sub

    ''' <summary>
    ''' Register presence of a new crop
    ''' </summary>
    <EventHandler()> _
    Public Sub OnNewCrop(newCrop As NewCropType)
        Dim senderIdx As Integer = FindComponentIndex(newCrop.sender)

        ' If sender is unknown, add it to the list
        If senderIdx < 0 Then
            Array.Resize(Of ComponentDataStruct)(ComponentData, ComponentData.Length + 1)
            senderIdx = ComponentData.Length - 1
            ComponentData(senderIdx).Name = newCrop.sender.ToLower()
            ComponentData(senderIdx).layerLAI = New Double(-1) {}
            ComponentData(senderIdx).layerLAItot = New Double(-1) {}
            ComponentData(senderIdx).Ftot = New Double(-1) {}
            ComponentData(senderIdx).Fgreen = New Double(-1) {}
            ComponentData(senderIdx).Rs = New Double(-1) {}
            ComponentData(senderIdx).Rl = New Double(-1) {}
            ComponentData(senderIdx).Rsoil = New Double(-1) {}
            ComponentData(senderIdx).Gc = New Double(-1) {}
            ComponentData(senderIdx).Ga = New Double(-1) {}
            ComponentData(senderIdx).PET = New Double(-1) {}
            ComponentData(senderIdx).PETr = New Double(-1) {}
            ComponentData(senderIdx).PETa = New Double(-1) {}
            ComponentData(senderIdx).Omega = New Double(-1) {}
            ' Fortran code added gsmax_* and dlt_gsmax_* properties here, but we can't do that!
            ComponentData(senderIdx).interception = New Double(-1) {}
        End If
        ComponentData(senderIdx).Type = newCrop.crop_type
        ' Read component specific constants;
        ComponentConstants(senderIdx)
    End Sub

    <EventHandler()> _
    Public Sub OnNew_Canopy(newCanopy As NewCanopyType)
        Dim senderIdx As Integer = FindComponentIndex(newCanopy.sender)
        If senderIdx < 0 Then
            Throw New Exception("Unknown Canopy Component: " & Convert.ToString(newCanopy.sender))
        End If
        ComponentData(senderIdx).LAI = newCanopy.lai
        ComponentData(senderIdx).LAItot = newCanopy.lai_tot
        ComponentData(senderIdx).CoverGreen = newCanopy.cover
        ComponentData(senderIdx).CoverTot = newCanopy.cover_tot
        ComponentData(senderIdx).Height = Math.Round(newCanopy.height, 5) / 1000.0 ' Round off a bit and convert mm to m
        ComponentData(senderIdx).Depth = Math.Round(newCanopy.depth, 5) / 1000.0 ' Round off a bit and convert mm to m
    End Sub

    ''' <summary>
    ''' Obtain updated information about a plant's growth capacity
    ''' </summary>
    <EventHandler()> _
    Public Sub OnNewPotentialGrowth(newPotentialGrowth As NewPotentialGrowthType)
        Dim senderIdx As Integer = FindComponentIndex(newPotentialGrowth.sender)
        If senderIdx < 0 Then
            Throw New Exception("Unknown Canopy Component: " & Convert.ToString(newPotentialGrowth.sender))
        End If
        ComponentData(senderIdx).Frgr = newPotentialGrowth.frgr
    End Sub

    <EventHandler()> _
    Public Sub OnInit2()
        Console.WriteLine("     Initialising")
        Console.WriteLine("     ")
        Console.WriteLine("        - Reading Constants")
        Console.WriteLine("     ")
        Console.WriteLine("        - Reading Parameters")
        Dim val As New DoubleType()
        ParentComponent().Get("latitude", val, False)
        latitude = val.Value
    End Sub


#End Region

#Region "Useful constants"
    Private Const svp_A As Double = 6.106    ' Teten coefficients
    Private Const svp_B As Double = 17.27    ' Teten coefficients
    Private Const svp_C As Double = 237.3    ' Teten coefficients
    Private Const abs_temp As Double = 273.16    ' 0 C in Kelvin (g_k)
    Private Const r_gas As Double = 8.3143    ' universal gas constant (J/mol/K)
    Private Const mwh2o As Double = 0.018016    ' molecular weight water (kg/mol)
    Private Const mwair As Double = 0.02897    ' molecular weight air (kg/mol)
    Private Const molef As Double = mwh2o / mwair    ' molecular fraction of water to air ()
    Private Const Cp As Double = 1010.0    ' Specific heat of air at constant pressure (J/kg/K)
    Private Const stef_boltz As Double = 0.0000000567    ' Stefan-Boltzman constant
    Private Const c_cloud As Double = 0.1    ' constant for cloud effect on longwave radiation
    Private Const Deg2Rad As Double = Math.PI / 180.0    ' convert degrees to radians
    Private Const RhoW As Double = 998.0    ' kg/m3
    Private Const svp_fract As Double = 0.66    ' weights vpd towards vpd at maximum temperature
    Private Const SunSetAngle As Double = 0.0
    Private Const hr2s As Double = 60.0 * 60.0    ' hours to seconds
#End Region
#Region "Various class variables"

    Private Structure ComponentDataStruct
        Public Name As String
        Public Type As String
        Public LAI As Double
        Public LAItot As Double
        Public CoverGreen As Double
        Public CoverTot As Double
        Public Ktot As Double
        Public K As Double
        Public Height As Double
        Public Depth As Double
        Public Albedo As Double
        Public Emissivity As Double
        Public Gsmax As Double
        Public R50 As Double
        Public Frgr As Double
        Public layerLAI As Double()
        Public layerLAItot As Double()
        Public Ftot As Double()
        Public Fgreen As Double()
        Public Rs As Double()
        Public Rl As Double()
        Public Rsoil As Double()
        Public Gc As Double()
        Public Ga As Double()
        Public PET As Double()
        Public PETr As Double()
        Public PETa As Double()
        Public Omega As Double()
        Public interception As Double()
    End Structure

    Private latitude As Double
    Private maxt As Double
    Private mint As Double
    Private radn As Double
    Private rain As Double
    Private vp As Double
    Private use_external_windspeed As Boolean
    Private windspeed_checked As Boolean = False

    Private day As Integer
    Private year As Integer

    Private netLongWave As Double
    Private sumRs As Double
    Private averageT As Double
    Private sunshineHours As Double
    Private fractionClearSky As Double
    Private dayLength As Double
    Private dayLengthLight As Double
    Private DeltaZ As Double() = New Double(-1) {}
    Private layerKtot As Double() = New Double(-1) {}
    Private layerLAIsum As Double() = New Double(-1) {}
    Private numLayers As Integer
    Private todayHeaderWritten As Boolean = False

    Private ComponentData As ComponentDataStruct() = New ComponentDataStruct(-1) {}

#End Region

    Private Function FetchTableValue(field As String, compNo As Integer, layerNo As Integer) As Double
        If field = "LAI" Then
            Return ComponentData(compNo).layerLAI(layerNo)
        ElseIf field = "Ftot" Then
            Return ComponentData(compNo).Ftot(layerNo)
        ElseIf field = "Fgreen" Then
            Return ComponentData(compNo).Fgreen(layerNo)
        ElseIf field = "Rs" Then
            Return ComponentData(compNo).Rs(layerNo)
        ElseIf field = "Rl" Then
            Return ComponentData(compNo).Rl(layerNo)
        ElseIf field = "Gc" Then
            Return ComponentData(compNo).Gc(layerNo)
        ElseIf field = "Ga" Then
            Return ComponentData(compNo).Ga(layerNo)
        ElseIf field = "PET" Then
            Return ComponentData(compNo).PET(layerNo)
        ElseIf field = "PETr" Then
            Return ComponentData(compNo).PETr(layerNo)
        ElseIf field = "PETa" Then
            Return ComponentData(compNo).PETa(layerNo)
        ElseIf field = "Omega" Then
            Return ComponentData(compNo).Omega(layerNo)
        Else
            Throw New Exception("Unknown table element: " & field)
        End If
    End Function

    Private Sub WriteTable(title As String)
        If Not todayHeaderWritten Then
            Dim Today As New DateTime(year, 1, 1)
            Today = Today.AddDays(day - 1)
            Console.WriteLine(Today.ToString("d MMMM yyyy") & "(Day of year=" & day.ToString() & "), " & Name & ": ")
            todayHeaderWritten = False
        End If

        Dim lineRule As String = "          " & New String("-"c, 70)
        Console.WriteLine("     Table for " & title)
        Console.WriteLine(lineRule)
        Console.Write("          Canopy Layer Height    ")
        For j As Integer = 0 To ComponentData.Length - 1
            Console.Write(ComponentData(j).Name.PadRight(10))
        Next
        Console.WriteLine("     Total")
        Console.WriteLine(lineRule)

        Dim compTot As Double() = New Double(ComponentData.Length - 1) {}
        Dim grandTot As Double = 0.0
        For i As Integer = numLayers - 1 To 0 Step -1
            Dim top As Double = 0.0
            Dim bottom As Double
            For layer As Integer = 0 To i
                top += DeltaZ(layer)
            Next
            If i = 0 Then
                bottom = 0.0
            Else
                bottom = top - DeltaZ(i)
            End If
            Dim layerTot As Double = 0.0

            Console.Write("      {0,7:F3} - {1,7:F3}     ", bottom, top)
            For j As Integer = 0 To ComponentData.Length - 1
                Dim val As Double = FetchTableValue(title, j, i)
                Console.Write("{0,10:F3}", val)
                layerTot += val
                compTot(j) += val
                grandTot += val
            Next
            Console.WriteLine("{0,10:F3}", layerTot)
        Next
        Console.WriteLine(lineRule)
        Console.Write("             Total          ")
        For j As Integer = 0 To ComponentData.Length - 1
            Console.Write("{0,10:F3}", compTot(j))
        Next
        Console.WriteLine("{0,10:F3}", grandTot)
        Console.WriteLine(lineRule)

    End Sub

    Private Function FindComponentIndex(name As String) As Integer
        For i As Integer = 0 To ComponentData.Length - 1
            If ComponentData(i).Name = name.ToLower() Then
                Return i
            End If
        Next
        ' Couldn't find
        Return -1
    End Function

    Private Sub ComponentConstants(idx As Integer)
        Dim targetType As String = ComponentData(idx).Type.ToLower()

        ' Apply the "default" values, then see whether they need to be overridden
        ComponentData(idx).Albedo = Me.albedo
        ComponentData(idx).Emissivity = Me.emissivity
        ComponentData(idx).R50 = Me.r50
        ComponentData(idx).Gsmax = Me.gsmax

        Dim cropType As New CropType()
        For i As Integer = 0 To Children.Count - 1
            If Children(i).[GetType]().Equals(cropType.[GetType]()) Then
                cropType = DirectCast(Children(i), CropType)
                If cropType.apply_to IsNot Nothing Then
                    For j As Integer = 0 To cropType.apply_to.Length - 1
                        If cropType.apply_to(j).ToLower() = targetType Then
                            If cropType.albedo >= 0.0 Then
                                ComponentData(idx).Albedo = cropType.albedo
                            End If
                            If cropType.emissivity >= 0.0 Then
                                ComponentData(idx).Emissivity = cropType.emissivity
                            End If
                            If cropType.r50 >= 0.0 Then
                                ComponentData(idx).R50 = cropType.r50
                            End If
                            If cropType.gsmax >= 0.0 Then
                                ComponentData(idx).Gsmax = cropType.gsmax
                            End If
                            Return
                        End If
                    Next
                End If
            End If
        Next
    End Sub


    Private Sub CanopyCompartments()
        DefineLayers()
        DivideComponents()
        LightExtinction()
    End Sub

    Private Sub MetVariables()
        ' averageT = (maxt + mint) / 2.0;
        averageT = CalcAverageT(mint, maxt)

        ' This is the length of time within the day during which
        '  Evaporation will take place
        dayLength = CalcDayLength(latitude, day, sun_angle)

        ' This is the length of time within the day during which
        ' the sun is above the horizon
        dayLengthLight = CalcDayLength(latitude, day, SunSetAngle)

        sunshineHours = CalcSunshineHours(radn, dayLengthLight, latitude, day)

        fractionClearSky = MathUtility.Divide(sunshineHours, dayLengthLight, 0.0)
    End Sub

    ''' <summary>
    ''' Break the combined Canopy into layers
    ''' </summary>
    Private Sub DefineLayers()
        Dim nodes As Double() = New Double(2 * ComponentData.Length - 1) {}
        Dim numNodes As Integer = 1
        For compNo As Integer = 0 To ComponentData.Length - 1
            Dim height As Double = ComponentData(compNo).Height
            Dim canopyBase As Double = height - ComponentData(compNo).Depth
            If Array.IndexOf(nodes, height) = -1 Then
                nodes(numNodes) = height
                numNodes = numNodes + 1
            End If
            If Array.IndexOf(nodes, canopyBase) = -1 Then
                nodes(numNodes) = canopyBase
                numNodes = numNodes + 1
            End If
        Next
        Array.Resize(Of Double)(nodes, numNodes)
        Array.Sort(nodes)
        numLayers = numNodes - 1
        If DeltaZ.Length <> numLayers Then
            ' Number of layers has changed; adjust array lengths
            Array.Resize(Of Double)(DeltaZ, numLayers)
            Array.Resize(Of Double)(layerKtot, numLayers)
            Array.Resize(Of Double)(layerLAIsum, numLayers)

            For j As Integer = 0 To ComponentData.Length - 1
                Array.Resize(Of Double)(ComponentData(j).Ftot, numLayers)
                Array.Resize(Of Double)(ComponentData(j).Fgreen, numLayers)
                Array.Resize(Of Double)(ComponentData(j).Rs, numLayers)
                Array.Resize(Of Double)(ComponentData(j).Rl, numLayers)
                Array.Resize(Of Double)(ComponentData(j).Rsoil, numLayers)
                Array.Resize(Of Double)(ComponentData(j).Gc, numLayers)
                Array.Resize(Of Double)(ComponentData(j).Ga, numLayers)
                Array.Resize(Of Double)(ComponentData(j).PET, numLayers)
                Array.Resize(Of Double)(ComponentData(j).PETr, numLayers)
                Array.Resize(Of Double)(ComponentData(j).PETa, numLayers)
                Array.Resize(Of Double)(ComponentData(j).Omega, numLayers)
                Array.Resize(Of Double)(ComponentData(j).interception, numLayers)
            Next
        End If
        For i As Integer = 0 To numNodes - 2
            DeltaZ(i) = nodes(i + 1) - nodes(i)
        Next
    End Sub

    ''' <summary>
    ''' Break the components into layers
    ''' </summary>
    Private Sub DivideComponents()
        Dim Ld As Double() = New Double(ComponentData.Length - 1) {}
        For j As Integer = 0 To ComponentData.Length - 1
            ComponentData(j).layerLAI = New Double(numLayers - 1) {}
            ComponentData(j).layerLAItot = New Double(numLayers - 1) {}
            Ld(j) = MathUtility.Divide(ComponentData(j).LAItot, ComponentData(j).Depth, 0.0)
        Next
        Dim top As Double = 0.0
        Dim bottom As Double = 0.0

        For i As Integer = 0 To numLayers - 1
            bottom = top
            top = top + DeltaZ(i)
            layerLAIsum(i) = 0.0

            ' Calculate LAI for layer i and component j
            ' ===========================================
            For j As Integer = 0 To ComponentData.Length - 1
                If (ComponentData(j).Height > bottom) AndAlso (ComponentData(j).Height - ComponentData(j).Depth < top) Then
                    ComponentData(j).layerLAItot(i) = Ld(j) * DeltaZ(i)
                    ComponentData(j).layerLAI(i) = ComponentData(j).layerLAItot(i) * _
                                                   MathUtility.Divide(ComponentData(j).LAI, ComponentData(j).LAItot, 0.0)
                    layerLAIsum(i) += ComponentData(j).layerLAItot(i)
                End If
            Next

            ' Calculate fractional contribution for layer i and component j
            ' ====================================================================
            For j As Integer = 0 To ComponentData.Length - 1
                ComponentData(j).Ftot(i) = MathUtility.Divide(ComponentData(j).layerLAItot(i), layerLAIsum(i), 0.0)
                ' Note: Sum of Fgreen will be < 1 as it is green over total
                ComponentData(j).Fgreen(i) = MathUtility.Divide(ComponentData(j).layerLAI(i), layerLAIsum(i), 0.0)
            Next
        Next
    End Sub

    ''' <summary>
    ''' Calculate light extinction parameters
    ''' </summary>
    Private Sub LightExtinction()
        ' Calculate effective K from LAI and cover
        ' =========================================
        For j As Integer = 0 To ComponentData.Length - 1
            If MathUtility.FloatsAreEqual(ComponentData(j).CoverGreen, 1.0, 0.00001) Then
                Throw New Exception("Unrealistically high cover value in MicroMet i.e. > -.9999")
            End If

            ComponentData(j).K = MathUtility.Divide(-Math.Log(1.0 - ComponentData(j).CoverGreen), ComponentData(j).LAI, 0.0)
            ComponentData(j).Ktot = MathUtility.Divide(-Math.Log(1.0 - ComponentData(j).CoverTot), ComponentData(j).LAItot, 0.0)
        Next

        ' Calculate extinction for individual layers
        ' ============================================
        For i As Integer = 0 To numLayers - 1
            layerKtot(i) = 0.0
            For j As Integer = 0 To ComponentData.Length - 1
                layerKtot(i) += ComponentData(j).Ftot(i) * ComponentData(j).Ktot

            Next
        Next
    End Sub

    ''' <summary>
    ''' Perform the overall Canopy Energy Balance
    ''' </summary>
    Private Sub BalanceCanopyEnergy()
        ShortWaveRadiation()
        EnergyTerms()
        LongWaveRadiation()
        SoilHeatRadiation()
    End Sub

    ''' <summary>
    ''' Calculate the canopy conductance for system compartments
    ''' </summary>
    Private Sub CalculateGc()
        Dim Rin As Double = radn

        For i As Integer = numLayers - 1 To 0 Step -1
            Dim Rflux As Double = Rin * 1000000.0 / (dayLength * hr2s) * (1.0 - albedo)
            Dim Rint As Double = 0.0

            For j As Integer = 0 To ComponentData.Length - 1
                ComponentData(j).Gc(i) = CanopyConductance(ComponentData(j).Gsmax, _
                                                           ComponentData(j).R50, _
                                                           ComponentData(j).Frgr, _
                                                           ComponentData(j).Fgreen(i), _
                                                           layerKtot(i), _
                                                           layerLAIsum(i), _
                                                           Rflux)

                Rint += ComponentData(j).Rs(i)
            Next
            ' Calculate Rin for the next layer down
            Rin -= Rint
        Next
    End Sub
    ''' <summary>
    ''' Calculate the aerodynamic conductance for system compartments
    ''' </summary>
    Private Sub CalculateGa()
        If Not windspeed_checked Then
            Dim val As New DoubleType()
            val.Value = [Double].NaN ' Set to NaN so we can test for non-receipt of value
            use_external_windspeed = ParentComponent().[Get]("windspeed", val, True) AndAlso (Not [Double].IsNaN(val.Value))
            windspeed = val.Value
            windspeed_checked = True
        End If
        If Not use_external_windspeed Then
            windspeed = windspeed_default
        End If

        Dim sumDeltaZ As Double = 0.0
        Dim sumLAI As Double = 0.0
        For i As Integer = 0 To numLayers - 1
            sumDeltaZ += DeltaZ(i)
            ' top height
            ' total lai
            sumLAI += layerLAIsum(i)
        Next

        Dim totalGa As Double = AerodynamicConductanceFAO(windspeed, refheight, sumDeltaZ, sumLAI)

        For i As Integer = 0 To numLayers - 1
            For j As Integer = 0 To ComponentData.Length - 1
                ComponentData(j).Ga(i) = totalGa * MathUtility.Divide(ComponentData(j).Rs(i), sumRs, 0.0)
            Next
        Next
    End Sub

    ''' <summary>
    ''' Calculate the interception loss of water from the canopy
    ''' </summary>
    Private Sub CalculateInterception()
        Dim sumLAI As Double = 0.0
        Dim sumLAItot As Double = 0.0
        For i As Integer = 0 To numLayers - 1
            For j As Integer = 0 To ComponentData.Length - 1
                sumLAI += ComponentData(j).layerLAI(i)
                sumLAItot += ComponentData(j).layerLAItot(i)
            Next
        Next

        Dim totalInterception As Double = a_interception * Math.Pow(rain, b_interception) + c_interception * sumLAItot + d_interception

        totalInterception = Math.Max(0.0, Math.Min(0.99 * rain, totalInterception))

        For i As Integer = 0 To numLayers - 1
            For j As Integer = 0 To ComponentData.Length - 1
                ComponentData(j).interception(i) = MathUtility.Divide(ComponentData(j).layerLAI(i), sumLAI, 0.0) * totalInterception
            Next
        Next
    End Sub

    ''' <summary>
    ''' Calculate the Penman-Monteith water demand
    ''' </summary>
    Private Sub CalculatePM()
        ' zero a few things, and sum a few others
        Dim sumRl As Double = 0.0
        Dim sumRsoil As Double = 0.0
        Dim sumInterception As Double = 0.0
        Dim freeEvapGa As Double = 0.0
        For i As Integer = 0 To numLayers - 1
            For j As Integer = 0 To ComponentData.Length - 1
                ComponentData(j).PET(i) = 0.0
                ComponentData(j).PETr(i) = 0.0
                ComponentData(j).PETa(i) = 0.0
                sumRl += ComponentData(j).Rl(i)
                sumRsoil += ComponentData(j).Rsoil(i)
                sumInterception += ComponentData(j).interception(i)
                freeEvapGa += ComponentData(j).Ga(i)
            Next
        Next

        Dim netRadiation As Double = ((1.0 - albedo) * sumRs + sumRl + sumRsoil) * 1000000.0  ' MJ/J
        netRadiation = Math.Max(0.0, netRadiation)
        Dim freeEvapGc As Double = freeEvapGa * 1000000.0   ' =infinite surface conductance
        Dim freeEvap As Double = CalcPenmanMonteith(netRadiation, mint, maxt, vp, air_pressure, dayLength, _
         freeEvapGa, freeEvapGc)

        dryleaffraction = 1.0 - MathUtility.Divide(sumInterception * (1.0 - night_interception_fraction), freeEvap, 0.0)
        dryleaffraction = Math.Max(0.0, dryleaffraction)

        For i As Integer = 0 To numLayers - 1
            For j As Integer = 0 To ComponentData.Length - 1
                netRadiation = 1000000.0 * ((1.0 - albedo) * ComponentData(j).Rs(i) + _
                                                 ComponentData(j).Rl(i) + _
                                                 ComponentData(j).Rsoil(i))
                                                 ' MJ/J
                netRadiation = Math.Max(0.0, netRadiation)

                ComponentData(j).PETr(i) = CalcPETr(netRadiation * dryleaffraction, mint, maxt, air_pressure, _
                                                    ComponentData(j).Ga(i), ComponentData(j).Gc(i))

                ComponentData(j).PETa(i) = CalcPETa(mint, maxt, vp, air_pressure, dayLength * dryleaffraction, _
                                                    ComponentData(j).Ga(i), ComponentData(j).Gc(i))

                ComponentData(j).PET(i) = ComponentData(j).PETr(i) + ComponentData(j).PETa(i)
            Next
        Next
    End Sub

    ''' <summary>
    ''' Calculate the aerodynamic decoupling for system compartments
    ''' </summary>
    Private Sub CalculateOmega()
        For i As Integer = 0 To numLayers - 1
            For j As Integer = 0 To ComponentData.Length - 1
                ComponentData(j).Omega(i) = CalcOmega(mint, maxt, air_pressure, ComponentData(j).Ga(i), ComponentData(j).Gc(i))
            Next
        Next
    End Sub

    ''' <summary>
    ''' Send an energy balance event
    ''' </summary>
    Private Sub SendEnergyBalanceEvent()
        Dim lightProfile As New CanopyEnergyBalanceType()
        Array.Resize(Of CanopyEnergyBalanceInterceptionType)(lightProfile.Interception, ComponentData.Length)
        For j As Integer = 0 To ComponentData.Length - 1
            lightProfile.Interception(j) = New CanopyEnergyBalanceInterceptionType()
            lightProfile.Interception(j).name = ComponentData(j).Name
            lightProfile.Interception(j).CropType = ComponentData(j).Type
            Array.Resize(Of CanopyEnergyBalanceInterceptionlayerType)(lightProfile.Interception(j).layer, numLayers)
            For i As Integer = 0 To numLayers - 1
                lightProfile.Interception(j).layer(i) = New CanopyEnergyBalanceInterceptionlayerType()
                lightProfile.Interception(j).layer(i).thickness = CSng(DeltaZ(i))
                lightProfile.Interception(j).layer(i).amount = CSng(ComponentData(j).Rs(i) * RadnGreenFraction(j))
            Next
        Next
        lightProfile.transmission = 0
        RaiseEvent Canopy_Energy_Balance(lightProfile)
    End Sub

    ''' <summary>
    ''' Send an water balance event
    ''' </summary>
    Private Sub SendWaterBalanceEvent()
        Dim waterBalance As New CanopyWaterBalanceType()
        Array.Resize(Of CanopyWaterBalanceCanopyType)(waterBalance.Canopy, ComponentData.Length)
        Dim totalInterception As Double = 0.0
        For j As Integer = 0 To ComponentData.Length - 1
            waterBalance.Canopy(j) = New CanopyWaterBalanceCanopyType()
            waterBalance.Canopy(j).name = ComponentData(j).Name
            waterBalance.Canopy(j).CropType = ComponentData(j).Type
            waterBalance.Canopy(j).PotentialEp = 0
            For i As Integer = 0 To numLayers - 1
                waterBalance.Canopy(j).PotentialEp += CSng(ComponentData(j).PET(i))
                totalInterception += ComponentData(j).interception(i)
            Next
        Next

        waterBalance.eo = 0.0F
        ' need to implement this later
        waterBalance.interception = CSng(totalInterception)
        RaiseEvent Canopy_Water_Balance(waterBalance)

    End Sub

End Class


''' <summary>
''' Helper class which reads the parameters for various crop types
''' from the .xml file. The application of these to override default values
''' occurs in MicroMet.ComponentConstants()
''' </summary>
''' <remarks></remarks>
Public Class CropType
    Inherits Instance
    <Param([Optional]:=True, MinVal:=0.0, MaxVal:=1.0)> _
    <Units("0-1")> _
    <Description("")> _
    Public albedo As Double = -1

    <Param([Optional]:=True, MinVal:=0.9, MaxVal:=1.0)> _
    <Units("0-1")> _
    <Description("")> _
    Public emissivity As Double = -1

    <Param([Optional]:=True, MinVal:=0.0, MaxVal:=1.0)> _
    <Units("m/s")> _
    <Description("")> _
    Public gsmax As Double = -1

    <Param([Optional]:=True, MinVal:=0.0, MaxVal:=1000.0)> _
    <Units("W/m^2")> _
    <Description("")> _
    Public r50 As Double = -1

    <Param([Optional]:=True)> _
    Public apply_to As String() = Nothing
End Class
