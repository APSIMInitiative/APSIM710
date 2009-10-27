Imports System
Imports System.Collections.Generic
Imports System.Text
Imports ModelFramework
' Note: typing three consecutive comment characters (e.g.''') above a sub or function will generate xml documentation.

''' <summary>
''' Since temperature changes rapidly near the soil surface and very little at depth, the best simulation will
''' be obtained with short elements (shallow layers) near the soil surface and longer ones deeper in the soil.
''' The element lengths should go in a geometric progression. Ten to twelve nodes are probably sufficient for
''' short term simulations (daily or weekly). Fifteen nodes would probably be sufficient for annual cycle simulation
''' where a deeper grid is needed. 
''' p36, Campbell, G.S. (1985) "Soil physics with BASIC: Transport models for soil-plant systems" (Amsterdam, Elsevier)
''' </summary>

Public Class SoilTempdotNET
    Inherits Instance
    '------------------------------------------------------------------------------------------------------------
    '-----------------------------------------------IMPORTANT NOTE-----------------------------------------------
    ' Due to FORTRAN's 'flexibility' with arrays there have been a few modifications to array sizes in this
    ' version of SoilTemp. All arrays here are forceably 0-based, so to deal with the fact that this module
    ' contains both 0- and 1-based arrays all arrays have been increased in size by 1. All indexing will not
    ' change from FORTRAN code, those arrays that are 1-based will simply not use their first (0th) element.
    '
    ' ALL 0-BASED ARRAYS WILL BE SUFFIXED WITH '_zb' IE: 'therm_cond' -> 'therm_cond_zb'
    '
    '------------------------------------------------------------------------------------------------------------

#Region "Constants"
    Private doInit1Stuff As Boolean = False      'NEW
    ' Two representations are distinguished for the profile.
    ' a) Soil layers, each with a top and bottom boundary and a depth. Index range of 1..NumLayer
    ' b) Temperature nodes, each node being at the centre of each layer and additionally a node below 
    '    the bottom layer and a node above the top layer (in the atmosphere). Index range of 0..NumLayer + 1,
    '    giving 0 for atmosphere,

    Const LAMBDA As Double = 2465000.0      'latent heat of vapourisation for water (J/kg); 1 mm evap/day = 1 kg/m^2 day requires 2.45*10^6 J/m^2
    Const STEFAN_BOLTZMANNconst As Double = 0.0000000567   ' defines the power per unit area emitted by a black body as a function of its thermodynamic temperature (W/m^2/K^4).
    Const DAYSinYear As Double = 365.25    ' no of days in one year
    Const DEG2RAD As Double = Math.PI / 180.0
    Const RAD2DEG As Double = 180.0 / Math.PI
    Const DOY2RAD As Double = DEG2RAD * 360.0 / DAYSinYear          ' convert day of year to radians
    Const MIN2SEC As Double = 60.0          ' 60 seconds in a minute - conversion minutes to seconds
    Const HR2MIN As Double = 60.0           ' 60 minutes in an hour - conversion hours to minutes
    Const SEC2HR As Double = 1.0 / (HR2MIN * MIN2SEC)  ' conversion seconds to hours
    Const DAYhrs As Double = 24.0           ' hours in a day
    Const DAYmins As Double = DAYhrs * HR2MIN           ' minutes in a day
    Const DAYsecs As Double = DAYmins * MIN2SEC         ' seconds in a day
    Const M2MM As Double = 1000.0           ' 1000 mm in a metre -  conversion Metre to millimetre
    Const MM2M As Double = 1 / M2MM           ' 1000 mm in a metre -  conversion millimetre to Metre
    Const KM2M As Double = 1000.0           ' 1000 m in a kilometre -  conversion kilometre to Metre
    Const HR2SEC As Double = HR2MIN * MIN2SEC  ' conversion hours to seconds
    Const DAY2HR As Double = DAYhrs             ' conversion days to hours
    Const PA2HPA As Double = 1.0 / 100.0        ' conversion of air pressure pascals to hectopascals
    Const MJ2J As Double = 1000000.0            ' convert MJ to J
    Const J2MJ As Double = 1.0 / MJ2J            ' convert J to MJ
    Const AIRnode As Integer = 0                ' index of atomspheric node
    Const SURFACEnode As Integer = 1            ' index of surface node
    Const TOPSOILnode As Integer = 2            ' index of first soil layer node
#End Region

#Region "Global Variables"
    Private gDt As Double = 0.0             ' Internal timestep in seconds.
    Private gTimeOfDaySecs As Double = 0             ' Time of day in seconds from midnight.
    Private gNz As Integer = 0               'number of nodes
    Private gNumLayers As Integer = 0        'number of soil layers in profile
    'Private dt_max As Double = 0.0
    Private gC1() As Double         'soil solids thermal conductivity parameter
    Private gC2() As Double         'soil solids thermal conductivity parameter
    Private gC3() As Double         'soil solids thermal conductivity parameter
    Private gC4() As Double         'soil solids thermal conductivity parameter
    Private gVolSpecHeatSoil() As Double  ' Joules/m^3/K
    Private gT_zb() As Double       'soil temperature
    Private gThr_zb() As Double       'soil temperature
    Private gheatStorage() As Double     'CP; heat storage between nodes - index is same as upper node (J/s/K or W/K)
    Private gThermalConductance_zb() As Double 'K; conductance of element between nodes - index is same as upper node
    Private gThermConductivity_zb() As Double  'thermal conductivity (W/m^2/K)
    Private gBoundaryLayerConductanceIterations As Integer = 0  'Number of iterations to calculate atmosphere boundary layer conductance
    Private gBoundaryLayerConductance As Double = 0.0  'Average daily atmosphere boundary layer conductance
    Private gTNew_zb() As Double       'soil temperature at the end of this iteration (oC)
    Private gZ_zb() As Double          'node depths - get from water module, metres
    Private gAirT As Double = 0.0       ' Air temperature (oC)
    Private gMaxT As Double = 0.0       ' Max daily Air temperature (oC)
    Private gMinT As Double = 0.0       ' Min daily Air temperature (oC)
    Private gMaxTyesterday As Double = 0.0
    Private gMinTyesterday As Double = 0.0
    Private gSW() As Double             'volumetric water content (cc water / cc soil)
    Private gTAve As Double = 0.0       'annual average soil temperature (oC)
    Private gMinTsoil() As Double       'minimum soil temperature (oC)
    Private gMaxTsoil() As Double       'maximum soil temperature (oC)
    Private gTimeStepSec As Double = 0.0     'this will be set to timestep * 60 at the beginning of each calc. (seconds)
    Private gRhob() As Double               'bd (soil bulk density) is name of the APSIM var for rhob so set rhob = bd later (g/cc)
    Private gDLayer() As Double             ' APSIM soil layer depths as thickness of layers (mm)
    Private gInstrumHt As Double = 0.0      ' (m) height of instruments above soil surface
    Private gAirPressure As Double = 0.0    ' (hPa) Daily air pressure
    Private gWindSpeed As Double = 0.0      ' (km) daily wind run
    Private gEos As Double = 0.0            ' (mm) pot sevap after modification for green cover residue wt 
    Private gEo As Double = 0.0             ' (mm) pot evapotranspitation 
    Private gEs As Double = 0.0             ' actual soil evaporation (mm)
    Private gRadnNet As Double = 0.0        ' Net radiation per internal timestep (MJ)
    Private gCanopyHeight As Double = 0.0   ' (m) height of canopy above ground
    Private gSoilRoughnessHeight As Double = 0.0    ' (mm) height of soil roughness
    Private gClay() As Double               ' Proportion of clay in each layer of profile (0-1)

#End Region
#Region "[Input]s From APSIM"
    <Input()> Private surfaceom_cover As Double = 0.0
    <Input()> Private timestep As Integer = 0     ' timestep in minutes
    <Input()> Private tav As Double = 0.0         'NEW (replaced t_ave) //annual average soil temperature (oC)
    <Input()> Private sw() As Double              'volumetric water content (cc water / cc soil)
    '[Input]                                //FIXME - optional input
    Private gMaxT_time As Double = 0.0       'Time of maximum temperature in hours
    <Input()> Private mint As Double = 0.0    ' (oC) minimum daily temperature
    <Input()> Private maxt As Double = 0.0    ' (oC) maximum daily temperature
    <Input()> Private eos As Double = 0.0     ' (mm) pot sevap after modification for green cover residue wt
    <Input()> Private es As Double = 0.0      ' actual soil evaporation (mm)
    <Input()> Private day As Integer = 0    ' day of year
    <Input()> Private year As Integer = 0   ' year
    <Input()> Private latitude As Double = 0.0  ' latitude of site (decimal deg)
    <Input()> Private amp As Double = 0.0       ' average monthly amplitude of temperature (oC)
    <Input()> Private ll15_dep() As Double      ' 15 bar lower soil lower limit (mm)
    <Input()> Private sw_dep() As Double        ' soil water ad depth of water (mm)
    <Input()> Private salb As Double = 0.0      ' soil albedo (0-1)
    <Input()> Private radn As Double = 0.0      ' incoming shortwave radiation (MJ)
    <Input()> Private eo As Double = 0.0        ' potential evapotranspiration (mm)
    '    <Input()> _
    'Private cover_tot As Double = 0.0    ' (0-1) total surface cover
    '<Input()> _
    Private instrum_height As Double = 0.0    ' (m) height of instruments above ground
    '<Input()> _
    Private pa As Double = 0.0    ' (mb or hPa) air pressure at site
    '<Input()> _
    Private altitude As Double = 0.0    ' (m) altitude at site
    '<Input()> _
    Private wind As Double = 0.0    ' (km) daily wind run
    '<Input()> _
    Private Height As Double = 0.0    ' (mm) height of canopy above ground
#End Region
#Region "[Param]s From Files"
    <Param()> _
    Private clay() As Double
    <Param()> _
    Private nu As Double = 0.0                  'forward/backward differencing coefficient (0-1).
    ' A weighting factor which may range from 0 to 1. If nu=0, the flux is determined by the temperature difference
    ' at the beginning of the time step. The numerical procedure which results from this choice is called a forward 
    ' difference of explicit method. If nu=0.5, the average of the old and new temperatures is used to compute heat flux. 
    ' This is called a time-centered or Crank-Nicholson scheme.
    ' The equation for computing T(j+1) is implicit for this choice of nu (and any other except nu=0) since T(j+1) depends
    ' on the values of the new temperatures at the nodes i+1 and i-1. Most heat flow models use either nu=0 or nu=0.5.
    ' The best value for nu is determined by consideration of numerical stability and accuracy.
    ' The explicit scheme with nu=0 predicts more heat transfer between nodes than would actually occur, and can therefore
    ' become unstable if time steps are too large. Stable numerical solutions are only obtained when (Simonson, 1975) 
    ' deltaT < CH*(deltaZ)^2 / 2*lambda.
    ' When nu < 0.5, stable solutions to the heat flow problem will always be obtained, but if nu is too small, the solutions
    ' may oscillate. The reason for this is that the simulated heat transfer between nodes overshoots. On the next time step
    ' the excess heat must be transferrec back, so the predicted temperature at that node oscillates. On the other hand, if nu 
    ' is too large, the temperature difference will be too small and not enough heat will be transferred. Simulated temperatures 
    ' will never oscillate under these conditions, but the simulation will understimate heat flux. The best accuracy is obtained
    ' with nu around 0.4, while best stability is at nu=1. A typical compromise is nu=0.6.

    <Param()> _
    Private vol_spec_heat_clay As Double = 0.0  '[Joules*m-3*K-1]
    <Param()> _
    Private vol_spec_heat_om As Double = 0.0    '[Joules*m-3*K-1]
    <Param()> _
    Private vol_spec_heat_water As Double = 0.0 '[Joules*m-3*K-1]
    <Param()> _
    Private maxt_time_default As Double = 0.0
    '[Param]
    Private gAveTsoil() As Double  'FIXME - optional. Allow setting from set in manager or get from input //init to average soil temperature 
    <Param()> _
    Private bound_layer_cond_source As String = ""
    <Param()> _
    Private boundary_layer_cond As Double = 0.0
    <Param()> _
    Private boundary_layer_conductance_iterations As Double = 0    ' maximum number of iterations to calculate atmosphere boundary layer conductance
    <Param()> _
    Private net_radn_source As String = ""
    <Param()> _
    Private default_wind_speed As Double = 0.0      ' default wind speed (m/s)
    <Param()> _
    Private default_altitude As Double = 0.0    ' default altitude (m)
    <Param()> _
    Private default_instrum_height As Double = 0.0  ' default instrument height (m)
    <Param()> _
    Private bare_soil_height As Double = 0.0        ' roughness element height of bare soil (mm)

#End Region

#Region "[Output]s"
    <Output()> _
    <Units("(oC)")> _
    Private ReadOnly Property final_soil_temp() As Double()     ' Temperature at end of last time-step within a day - midnight
        Get
            Dim result(gNumLayers - 1) As Double
            Array.ConstrainedCopy(gT_zb, TOPSOILnode, result, 0, gNumLayers)
            Return result
        End Get
    End Property

    <Output()> _
    <Units("(oC)")> _
    Private ReadOnly Property final_soil_temp_surface() As Double   ' Temperature at end of last time-step within a day - midnight
        Get
            Return gT_zb(SURFACEnode)
        End Get
    End Property

    <Output()> _
    <Units("(oC)")> _
    Private ReadOnly Property ave_soil_temp() As Double()   ' Temperature averaged over all time-steps within a day
        Get
            'if called during init1 this will return an array of length 100 will all elements as '0'
            Dim result(gNumLayers - 1) As Double
            Array.ConstrainedCopy(gAveTsoil, TOPSOILnode, result, 0, gNumLayers)
            Return result
        End Get
    End Property

    <Output()> _
    <Units("(oC)")> _
    Private ReadOnly Property ave_soil_temp_surface() As Double     ' Temperature averaged over all time-steps within a day
        Get
            Return gAveTsoil(SURFACEnode)
        End Get
    End Property

    <Output()> _
    <Units("(oC)")> _
    Private ReadOnly Property mint_soil() As Double()
        Get
            Dim result(gNumLayers - 1) As Double
            Array.ConstrainedCopy(gMinTsoil, TOPSOILnode, result, 0, gNumLayers)
            Return result
        End Get
    End Property

    <Output()> _
    <Units("(oC)")> _
    Private ReadOnly Property mint_soil_surface() As Double
        Get
            Return gMinTsoil(SURFACEnode)
        End Get
    End Property

    <Output()> _
    <Units("(oC)")> _
    Private ReadOnly Property maxt_soil() As Double()
        Get
            Dim result(gNumLayers - 1) As Double
            Array.ConstrainedCopy(gMaxTsoil, TOPSOILnode, result, 0, gNumLayers)
            Return result
        End Get
    End Property

    <Output()> _
    <Units("(oC)")> _
    Private ReadOnly Property maxt_soil_surface() As Double
        Get
            Return gMaxTsoil(SURFACEnode)
        End Get
    End Property

    <Output()> _
    <Units("(J/sec/m/K)")> _
    Private ReadOnly Property boundary_layer_conductance() As Double
        Get
            Return gBoundaryLayerConductance
        End Get
    End Property
    <Output()> _
    <Units("(J/sec/m/K)")> _
    Private ReadOnly Property therm_cond() As Double()
        Get
            Dim result(gNz - 1) As Double
            Array.ConstrainedCopy(gThermConductivity_zb, 1, result, 0, gNz)
            Return result
        End Get
    End Property

    <Output()> _
    <Units("(J/m3/K/s)")> _
    Private ReadOnly Property heat_capacity() As Double()
        Get
            Dim result(gNz - 1) As Double
            Array.ConstrainedCopy(gVolSpecHeatSoil, SURFACEnode, result, 0, gNz)  'FIXME - should index be 2?
            Return result
        End Get
    End Property
    <Output()> _
    <Units("(J/m3/K/s)")> _
    Private ReadOnly Property heat_store() As Double()
        Get
            Dim result(gNz - 1) As Double
            Array.ConstrainedCopy(gheatStorage, SURFACEnode, result, 0, gNz)  'FIXME - should index be 2?
            Return result
        End Get
    End Property
    <Output()> _
    <Units("(oC)")> _
    Private ReadOnly Property Thr_profile() As Double()
        Get
            Dim result(gNz + 1) As Double
            gThr_zb.CopyTo(result, 0)
            Return result
        End Get
    End Property
#End Region

#Region "Event Handlers"
    <EventHandler()> _
    Public Sub OnInit1()
        Try
            doInit1Stuff = True
        Catch
            Throw New Exception("Init1 Failed!")
        End Try
    End Sub

    <EventHandler()> _
    Public Sub Onnew_profile(ByVal Soil As NewProfileType)
        getProfileVariables(Soil)
    End Sub

    <EventHandler()> _
    Public Sub Onnewmet(ByVal Met As NewMetType)
        getMetVariables(Met)
    End Sub

    <EventHandler()> _
    Public Sub OnnewCanopy(ByVal Canopy As NewCanopyType)
        getCanopyVariables(Canopy)
    End Sub

    <EventHandler()> _
    Public Sub OnProcess()

        gMaxT_time = maxt_time_default
        BoundCheck(gMaxT_time, 0.0, DAYhrs, "maxt_time")

        GetOtherVariables()       ' FIXME - note: Need to set yesterday's MaxTg and MinTg to today's at initialisation

        If doInit1Stuff Then        ' FIXME - need here until variable passing on init2 enabled
            'set t and tnew values to TAve. soil_temp is currently not used
            soiln2_soil_temp(gT_zb)
            gT_zb(AIRnode) = (gMaxT + gMinT) * 0.5
            gT_zb(SURFACEnode) = SurfaceTemperatureInit()
            gT_zb(gNz + 1) = tav
            'gT_zb(gNz + 1) = gT_zb(gNz)
            gT_zb.CopyTo(gTNew_zb, 0)

            ''gTAve = tav
            ''For node As Integer = AIRNODE To gNz + 1      ' FIXME - need here until variable passing on init2 enabled
            ''    gT_zb(node) = gTAve                  ' FIXME - need here until variable passing on init2 enabled
            ''    gTNew_zb(node) = gTAve                 ' FIXME - need here until variable passing on init2 enabled
            ''Next node
            gMaxTyesterday = maxt
            gMinTyesterday = mint
            doInit1Stuff = False
        Else
            ' Init1 stuff already done
        End If

        doProcess()
    End Sub 'OnProcess

#End Region

#Region "Initialisation"

    ''' <summary>
    ''' Initialise soiltemp module
    ''' </summary>
    <EventHandler()> _
    Public Sub OnInit2()            'JNGH - changed this from Init1.
        Console.WriteLine()
        Console.WriteLine("------- soiltemp Initialisation -----------------------------------------------")
        Console.WriteLine("     Initialising :")
        getIniVariables()
        readParam()
    End Sub 'OnInit2
    ''' <summary>
    ''' initialise global variables to initial values
    ''' </summary>
    ''' <remarks></remarks>
    Private Sub getIniVariables()

        BoundCheck(tav, -30.0, 40.0, "tav (oC)")
        ''gTAve = tav

        If (instrum_height > 0.00001) Then
            gInstrumHt = instrum_height
        Else
            gInstrumHt = default_instrum_height
        End If
        BoundCheck(gInstrumHt, 0.0, 5.0, "instrum_height (m)")

        BoundCheck(altitude, -100.0, 6000.0, "altitude (m)")
        Dim AltitudeMetres As Double = 0.0
        'If (altitude >= 0.0) Then
        '    AltitudeMetres = altitude
        'Else
        AltitudeMetres = default_altitude       ' FIXME - need to detect if altitude not supplied elsewhere.
        'End If

        gAirPressure = 101325.0 * Math.Pow((1.0 - 2.25577 * 0.00001 * AltitudeMetres), 5.25588) * PA2HPA
        BoundCheck(gAirPressure, 800.0, 1200.0, "air pressure (hPa)")

    End Sub 'getIniVariables
    ''' <summary>
    ''' set maximum and minimum temperatures
    ''' </summary>
    ''' <param name="Met"></param>
    ''' <remarks></remarks>
    Private Sub getMetVariables(ByRef Met As NewMetType)
        maxt = Met.maxt
        mint = Met.mint
    End Sub 'getMetVariables
    ''' <summary>
    ''' Set canopy height
    ''' </summary>
    ''' <param name="Canopy"></param>
    ''' <remarks></remarks>
    Private Sub getCanopyVariables(ByRef Canopy As NewCanopyType)
        Height = Canopy.height
    End Sub 'getCanopyVariables
    ''' <summary>
    ''' Set global variables to new soil profile state
    ''' </summary>
    ''' <param name="Soil"></param>
    ''' <remarks></remarks>
    Private Sub getProfileVariables(ByRef Soil As NewProfileType)
        Const CONSTANT_TEMPdepth As Double = 10.0    ' Metres. Depth to constant temperature zone
        're-dimension dlayer, bd. These will now be treated as '1-based' so 0th element will not be used
        gNumLayers = Soil.dlayer.Length
        gNz = gNumLayers + 1

        ReDim Preserve gDLayer(gNumLayers + 1)     ' Dlayer dimensioned for layers 1 to gNumlayers + 1 extra for zone below bottom layer
        Soil.dlayer.CopyTo(gDLayer, 1)
        BoundCheckArray(gDLayer, 0.0, 1000.0, "dlayer")

        ' mapping of layers to nodes -
        ' layer - air surface 1 2 ... NumLayers NumLayers+1
        ' node  - 0   1       2 3 ... Nz        Nz+1
        ' thus the node 1 is at the soil surface and Nz = NumLayers + 1.

        'add enough to make last node at 9-10 meters - should always be enough to assume constant temperature
        Dim BelowProfileDepth As Double = Math.Max(CONSTANT_TEMPdepth * M2MM - SumOfRange(gDLayer, 1, gNumLayers), 1.0 * M2MM)    ' Metres. Depth added below profile to take last node to constant temperature zone
        gDLayer(gNumLayers + 1) = BelowProfileDepth * 2.0       ' double depth so that node at mid-point is at the ConstantTempDepth


        ' Set the node depths to approx middle of soil layers
        ReDim Preserve gZ_zb(gNz + 1)      ' Z_zb dimensioned for nodes 0 to Nz + 1 extra for zone below bottom layer
        gZ_zb(AIRnode) = 0.0R
        gZ_zb(SURFACEnode) = 0.0R
        gZ_zb(TOPSOILnode) = 0.5 * gDLayer(1) * MM2M
        For node As Integer = TOPSOILnode To gNz
            gZ_zb(node + 1) = (SumOfRange(gDLayer, 1, node - 1) + 0.5 * gDLayer(node)) * MM2M
        Next node

        ' BD
        BoundCheck(Soil.bd.Length, gNumLayers, gNumLayers, "bd layers")
        ReDim Preserve gRhob(gNumLayers + 1)     ' Rhob dimensioned for layers 1 to gNumlayers + 1 extra for zone below bottom layer
        Soil.bd.CopyTo(gRhob, 1)
        BoundCheckArray(gRhob, 0.0, 2.65, "bd")
        'if bd (rhob) has more or less elements than the number of layers, throw exception

        gRhob(gNz) = gRhob(gNumLayers)
        ' Debug test: multiplyArray(gRhob, 2.0)

        ' SW
        BoundCheck(Soil.sw_dep.Length, gNumLayers, gNumLayers, "sw layers")
        ReDim Preserve gSW(gNumLayers + 1)     ' SW dimensioned for layers 1 to gNumlayers + 1 extra for zone below bottom layer
        For layer As Integer = 1 To gNumLayers
            gSW(layer) = Soil.sw_dep(layer - 1) / gDLayer(layer)
        Next layer
        gSW(gNz) = gSW(gNumLayers)

        ReDim Preserve gMaxTsoil(gNumLayers + 1)     ' MaxTsoil dimensioned for layers 1 to gNumlayers + 1 extra for zone below bottom layer
        ReDim Preserve gMinTsoil(gNumLayers + 1)     ' MinTsoil dimensioned for layers 1 to gNumlayers + 1 extra for zone below bottom layer
        ReDim Preserve gAveTsoil(gNumLayers + 1)     ' AveTsoil dimensioned for layers 1 to gNumlayers + 1 extra for zone below bottom layer
        ReDim Preserve gVolSpecHeatSoil(gNz)        ' HeatStore dimensioned for layers 1 to gNumlayers + 1 extra for zone below bottom layer
        ReDim Preserve gT_zb(gNz + 1)               ' T_zb dimensioned for nodes 0 to Nz + 1 extra for zone below bottom layer
        ReDim Preserve gThr_zb(gNz + 1)               ' Thr_zb dimensioned for nodes 0 to Nz + 1 extra for zone below bottom layer
        ReDim Preserve gTNew_zb(gNz + 1)            ' TNew_zb dimensioned for nodes 0 to Nz + 1 extra for zone below bottom layer
        ReDim Preserve gThermConductivity_zb(gNz)   ' ThermCond_zb dimensioned for nodes 0 to Nz + 1 extra for zone below bottom layer
        ReDim Preserve gheatStorage(gNz)     'CP; heat storage between nodes - index is same as upper node
        ReDim Preserve gThermalConductance_zb(gNz + 1) 'K; conductance between nodes - index is same as upper node

    End Sub 'getProfileVariables
    ''' <summary>
    ''' Set global variables with module parameter values and check validity
    ''' </summary>
    ''' <remarks></remarks>
    Private Sub readParam()
        '
        'clay//
        'if clay has more or less elements than the number of layers, throw exception
        BoundCheck(clay.Length, gNumLayers, gNumLayers, "clay layers")
        BoundCheckArray(clay, 0.000001, 1.0, "clay")

        ReDim Preserve gClay(gNumLayers + 1)     ' Clay dimensioned for layers 1 to gNumlayers + 1 extra for zone below bottom layer
        clay.CopyTo(gClay, 1)
        gClay(gNz) = gClay(gNumLayers)   ' Set zone below bottom layer to the same as bottom layer

        doThermalConductivityCoeffs()

        'set t and tn values to TAve. soil_temp is currently not used
        soiln2_soil_temp(gT_zb)     'FIXME - returns as zero here because initialisation is not complete.
        gT_zb.CopyTo(gTNew_zb, 0)

        BoundCheck(nu, 0.0, 1.0, "nu")
        BoundCheck(vol_spec_heat_om, 1000000.0, 10000000.0, "vol_spec_heat_om")
        BoundCheck(vol_spec_heat_water, 1000000.0, 10000000.0, "vol_spec_heat_water")
        BoundCheck(vol_spec_heat_clay, 1000000.0, 10000000.0, "vol_spec_heat_clay")
        BoundCheck(maxt_time_default, 0.0, DAYhrs, "maxt_time_default")
        BoundCheck(default_wind_speed, 0.0, 10.0, "default_wind_speed")
        BoundCheck(default_altitude, -100.0, 1200.0, "default_altitude")
        BoundCheck(default_instrum_height, 0.0, 5.0, "default_instrum_height")
        BoundCheck(bare_soil_height, 0.0, 150.0, "bare_soil_height")     ' "canopy height" when no canopy is present. FIXME - need to test if canopy is absent and set to this.
        gSoilRoughnessHeight = bare_soil_height
        Select Case bound_layer_cond_source.ToLower()
            Case "calc", "constant"
                ' ok, legal values
            Case Else
                Throw New Exception("bound_layer_cond_source (" + bound_layer_cond_source + ") must be either 'calc' or 'constant'")
        End Select
        BoundCheck(boundary_layer_conductance_iterations, 0, 10, "boundary_layer_conductance_iterations")
        BoundCheck(boundary_layer_cond, 10.0, 40.0, "boundary_layer_cond")
        gBoundaryLayerConductanceIterations = CInt(boundary_layer_conductance_iterations)
        Select Case net_radn_source.ToLower()
            Case "calc", "eos"
                ' ok, legal values
            Case Else
                Throw New Exception("net_radn_source (" + net_radn_source + ") must be either 'calc' or 'eos'")
        End Select
    End Sub 'readParam
    ''' <summary>
    ''' Calculate the coefficients for thermal conductivity equation (Campbell 4.20) for a typical low-quartz, mineral soil.
    ''' </summary>
    ''' <remarks></remarks>
    Private Sub doThermalConductivityCoeffs()
        ReDim Preserve gC1(gNz)     ' C1 dimensioned for nodes 0 to Nz
        ReDim Preserve gC2(gNz)     ' C2 dimensioned for nodes 0 to Nz
        ReDim Preserve gC3(gNz)     ' C3 dimensioned for nodes 0 to Nz
        ReDim Preserve gC3(gNz)     ' C3 dimensioned for nodes 0 to Nz
        ReDim Preserve gC4(gNz)     ' C4 dimensioned for nodes 0 to Nz

        For layer As Integer = 1 To gNumLayers + 1
            Dim element As Integer = layer
            ' first coefficient C1 - For many mineral soils, the quartz fractioncan be taken as nil, and the equation can
            ' be approximated by this equation - 4.27 Campbell.
            gC1(element) = 0.65 - 0.78 * gRhob(layer) + 0.6 * Math.Pow(gRhob(layer), 2)      'A approximation to e
            ' The coefficient C2 (B in Campbell 4.25) can be evaluated from data and for mineral soils is approximated by -
            gC2(element) = 1.06 * gRhob(layer)                              ' * SW[i]; //B for mineral soil - assume (is there missing text here??)
            ' The coefficient C3 (C in Caqmpbell 4.28) determines the water content where thermal conductivity begins to
            ' increase rapidly and is highly correlated with clay content. The following correlation appears to fit data well.
            gC3(element) = 1.0R + 2.6 / Math.Sqrt(gClay(layer))             'C is the water content where co (is there missing text here??)
            ' Coefficient C4 (D in Campbell 4.22) is the thermal conductivity when volumetric water content=0. 
            ' For mineral soils with a particle density of 2.65 Mg/m3 the equation becomes the following.
            gC4(element) = 0.03 + 0.1 * Math.Pow(gRhob(layer), 2)           'D assume mineral soil particle d (is there missing text here??)
        Next layer


    End Sub  'doThermalConductivityCoeffs

#End Region

#Region "daily comms"
    ''' <summary>
    ''' Update global variables with external states and check validity of values.
    ''' </summary>
    ''' <remarks></remarks>
    Private Sub GetOtherVariables()

        BoundCheck(maxt, mint, 100.0, "maxt")
        gMaxT = maxt

        BoundCheck(mint, -100.0, maxt, "mint")
        gMinT = mint

        BoundCheck(timestep, 0, CInt(DAYmins), "timestep")
        gTimeStepSec = CDbl(timestep) * MIN2SEC

        BoundCheckArray(sw, 0.0, 1.0, "sw")
        sw.CopyTo(gSW, 1)
        gSW(gNz) = gSW(gNumLayers)
        'Debug(test): multiplyArray(gSW, 0.1)

        BoundCheck(eo, -30.0, 40.0, "eo")
        gEo = eo

        BoundCheck(eos, -30.0, 40.0, "eos")
        gEos = eos

        BoundCheck(es, -30.0, 40.0, "es")
        gEs = es
        'BoundCheck(cover_tot, 0.0, 1.0, "cover_tot")

        If (wind > 0.0) Then
            gWindSpeed = wind * KM2M / (DAY2HR * HR2SEC)
        Else
            gWindSpeed = default_wind_speed
        End If
        BoundCheck(gWindSpeed, 0.0, 1000.0, "wind")

        gCanopyHeight = Math.Max(Height, gSoilRoughnessHeight) * MM2M
        BoundCheck(gCanopyHeight, 0.0, 20000.0, "Height")

    End Sub 'GetOtherVariables
#End Region

#Region "Calculation"

    ''' <summary>
    ''' Perform actions for current day
    ''' </summary>
    Private Sub doProcess()
        Const ITERATIONSperDAY As Integer = 48     ' number of iterations in a day

        Dim cva As Double = 0.0
        Dim cloudFr As Double = 0.0
        Dim solarRadn(ITERATIONSperDAY) As Double   ' Total incoming short wave solar radiation per timestep
        doNetRadiation(solarRadn, cloudFr, cva, ITERATIONSperDAY)

        'zero the temperature profiles
        SetArray(gMinTsoil, 0.0)
        SetArray(gMaxTsoil, 0.0)
        SetArray(gAveTsoil, 0.0)
        gBoundaryLayerConductance = 0.0

        Dim RnTot As Double = 0.0
        'calc dt
        gDt = Math.Round(gTimeStepSec / CDbl(ITERATIONSperDAY))
        For timeStepIteration As Integer = 1 To ITERATIONSperDAY
            gTimeOfDaySecs = gDt * CDbl(timeStepIteration)
            If gTimeStepSec < DAYsecs Then
                gAirT = 0.5 * (gMaxT + gMinT)
            Else
                gAirT = InterpTemp(gTimeOfDaySecs * SEC2HR)
            End If
            ' Convert to hours //most of the arguments in FORTRAN version are global vars so
            ' do not need to pass them here, they can be accessed inside InterpTemp
            gTNew_zb(AIRnode) = gAirT

            gRadnNet = radnNetInterpolate(solarRadn(timeStepIteration), cloudFr, cva)
            RnTot += gRadnNet   ' for debugging only

            doVolumetricSpecificHeat()      'RETURNS gVolSpecHeatSoil() (volumetric heat capacity of nodes)

            doThermConductivity()     'RETURNS gThermConductivity_zb()
            Select Case bound_layer_cond_source.ToLower()
                Case "constant"
                    gThermConductivity_zb(AIRnode) = boundaryLayerConductanceConst()
                Case "calc"
                    ' When calculating the boundary layer conductance it is important to iterate the entire
                    ' heat flow calculation at least once, since surface temperature depends on heat flux to 
                    ' the atmosphere, but heat flux to the atmosphere is determined, in part, by the surface
                    ' temperature.
                    gThermConductivity_zb(AIRnode) = boundaryLayerConductance(gTNew_zb)
                    For iteration As Integer = 1 To gBoundaryLayerConductanceIterations
                        doThomas(gTNew_zb)        'RETURNS TNew_zb()
                        gThermConductivity_zb(AIRnode) = boundaryLayerConductance(gTNew_zb)
                    Next iteration
            End Select
            ' Now start again with final atmosphere boundary layer conductance
            doThomas(gTNew_zb)        'RETURNS gTNew_zb()
            doUpdate(ITERATIONSperDAY)
            If (RealsAreEqual(gTimeOfDaySecs, 5.0 * HR2MIN * MIN2SEC)) Then
                gT_zb.CopyTo(gThr_zb, 0)
            End If
        Next timeStepIteration

        ' Next two for DEBUGGING only
        Dim RnByEos As Double = gEos * LAMBDA * J2MJ   ' Es*L = latent heat flux LE and Eos*L = net Radiation Rn.
        Dim LEbyEs As Double = gEs * LAMBDA * J2MJ   ' Es*L = latent heat flux LE and Eos*L = net Radiation Rn.

        gMinTyesterday = gMinT
        gMaxTyesterday = gMaxT
    End Sub 'Process

    ''' <summary>
    '''Calculate the volumetric specific heat (volumetric heat capicity Cv) of the soil layer
    '''to Campbell, G.S. (1985) "Soil physics with BASIC: Transport
    '''models for soil-plant systems" (Amsterdam, Elsevier)
    ''' RETURNS gVolSpecHeatSoil()  [Joules*m-3*K-1]
    ''' </summary>
    Private Sub doVolumetricSpecificHeat()
        Const SPECIFICbd As Double = 2.65   ' (g/cc) specific bulk density
        Dim volSpecHeatSoil(gNumLayers) As Double

        For layer As Integer = 1 To gNumLayers
            Dim solidity As Double = gRhob(layer) / SPECIFICbd

            'the Campbell version
            'heatStore[i] = (vol_spec_heat_clay * (1-porosity) + vol_spec_heat_water * SWg[i]) * (zb_z[i+1]-zb_z[i-1])/(2*real(dt));
            volSpecHeatSoil(layer) = vol_spec_heat_clay * solidity _
                                   + vol_spec_heat_water * gSW(layer)
        Next layer
        'mapLayer2Node(volSpecHeatSoil, gVolSpecHeatSoil)    
        volSpecHeatSoil.CopyTo(gVolSpecHeatSoil, 1)     ' map volumetric heat capicity (Cv) from layers to nodes (node 2 in centre of layer 1)
        gVolSpecHeatSoil(1) = volSpecHeatSoil(1)        ' assume surface node Cv is same as top layer Cv

    End Sub 'doVolumetricSpecificHeat

    ''' <summary>
    ''' Calculate the thermal conductivity of the soil layer following,
    ''' to Campbell, G.S. (1985) "Soil physics with BASIC: Transport
    ''' models for soil-plant systems" (Amsterdam, Elsevier)
    ''' Equation 4.20 where Lambda = A + B*Theta - (A-D)*exp[-(C*theta)^E]
    ''' Lambda is the thermal conductivity, theta is volumetric water content and A, B, C, D, E are coefficients.
    ''' When theta = 0, lambda = D. At saturation, the last term becomes zero and Lambda = A + B*theta.
    ''' ' The constant E can be assigned a value of 4. The constant C determines the water content where thermal
    ''' conductivity begins to increase rapidly and is highly correlated with clay content.
    ''' Here C1=A, C2=B, SW=theta, C3=C, C4=D, 4=E.
    ''' RETURNS gThermConductivity_zb() (W/m2/K)
    ''' </summary>
    Private Sub doThermConductivity()
        Dim temp As Double = 0

        ' no change needed from Campbell to my version
        ' Do thermal conductivity for soil layers
        Dim thermCondLayers(gNz) As Double
        For layer As Integer = 1 To gNz
            temp = Math.Pow((gC3(layer) * gSW(layer)), 4) * (-1)
            thermCondLayers(layer) = gC1(layer) + (gC2(layer) * gSW(layer)) _
                                   - (gC1(layer) - gC4(layer)) * Math.Exp(temp)  ' Eqn 4.20 Campbell.
        Next layer

        ' now get weighted average for soil elements between the nodes. i.e. map layers to nodes
        mapLayer2Node(thermCondLayers, gThermConductivity_zb)
    End Sub 'doThermConductivity
    Private Sub mapLayer2Node(ByVal layerArray() As Double, ByRef nodeArray() As Double)
        ' now get weighted average for soil elements between the nodes. i.e. map layers to nodes
        For node As Integer = SURFACEnode To gNz
            Dim layer As Integer = node - 1     ' node n lies at the centre of layer n-1
            Dim depthLayerAbove As Double = SumOfRange(gDLayer, 1, layer)
            Dim d1 As Double = depthLayerAbove - (gZ_zb(node) * M2MM)
            Dim d2 As Double = gZ_zb(node + 1) * M2MM - depthLayerAbove
            Dim dSum As Double = d1 + d2

            nodeArray(node) = (layerArray(layer) * d1 / dSum) _
                            + (layerArray(layer + 1) * d2 / dSum)
        Next node
    End Sub
    ''' <summary>
    ''' Numerical solution of the differential equations. Solves the
    ''' tri_diagonal matrix using the Thomas algorithm, Thomas, L.H. (1946)
    ''' "Elliptic problems in linear difference equations over a network"
    ''' Watson Sci Comput. Lab. Report., (Columbia University, New York)"
    ''' RETURNS gTNew_zb()
    ''' </summary>
    ''' <remarks>John Hargreaves' version from Campbell Program 4.1</remarks>
    Private Sub doThomas(ByRef TNew_zb() As Double)

        Dim a(gNz + 1) As Double    'A; thermal conductance at next node (W/m/K)
        Dim b(gNz) As Double        'B; heat storage at node (W/K)
        Dim c(gNz) As Double        'C; thermal conductance at node (W/m/K)
        Dim d(gNz) As Double        'D; heat flux at node (w/m) and then temperature
        ' nu = F; Nz = M; 1-nu = G; T_zb = T; TNew_zb = TN; 

        gThermalConductance_zb(AIRnode) = gThermConductivity_zb(AIRnode)
        ' The first node gZ_zb(1) is at the soil surface (Z = 0)
        For node As Integer = SURFACEnode To gNz
            Dim VolSoilAtNode As Double = 0.5 * (gZ_zb(node + 1) - gZ_zb(node - 1))   ' Volume of soil around node (m^3), assuming area is 1 m^2
            gheatStorage(node) = gVolSpecHeatSoil(node) * VolSoilAtNode / gDt       ' Joules/s/K or W/K
            'rate of heat
            'convert to thermal conductance
            Dim elementLength As Double = gZ_zb(node + 1) - gZ_zb(node)             ' (m)
            gThermalConductance_zb(node) = gThermConductivity_zb(node) / elementLength  ' (W/m/K)
        Next node

        ' Debug test: multiplyArray(gThermalConductance_zb, 2.0)

        'John's version
        Dim g As Double = 1 - nu
        For node As Integer = SURFACEnode To gNz
            c(node) = (-nu) * gThermalConductance_zb(node)   '  
            a(node + 1) = c(node)             ' Eqn 4.13
            b(node) = nu * (gThermalConductance_zb(node) + gThermalConductance_zb(node - 1)) + gheatStorage(node)    ' Eqn 4.12
            ' Eqn 4.14
            d(node) = g * gThermalConductance_zb(node - 1) * gT_zb(node - 1) _
                    + (gheatStorage(node) - g * (gThermalConductance_zb(node) + gThermalConductance_zb(node - 1))) * gT_zb(node) _
                    + g * gThermalConductance_zb(node) * gT_zb(node + 1)
        Next node
        a(SURFACEnode) = 0.0R

        ' The boundary condition at the soil surface is more complex since convection and radiation may be important.
        ' When radiative and latent heat transfer are unimportant, then D(1) = D(1) + nu*K(0)*TN(0).
        'd(SURFACEnode) += nu * gThermalConductance_zb(AIRnode) * TNew_zb(AIRnode)       ' Eqn. 4.16
        Dim sensibleHeatFlux As Double = nu * gThermalConductance_zb(AIRnode) * TNew_zb(AIRnode)       ' Eqn. 4.16

        ' When significant radiative and/or latent heat transfer occur they are added as heat sources at node 1
        ' to give D(1) = D(1) = + nu*K(0)*TN(0) - Rn + LE, where Rn is net radiation at soil surface and LE is the
        ' latent heat flux. Eqn. 4.17

        Dim RadnNet As Double = 0.0     ' (W/m)
        Select Case net_radn_source.ToLower()
            Case "calc"
                RadnNet = gRadnNet * MJ2J / gDt       ' net Radiation Rn heat flux (J/m2/s or W/m2).
            Case "eos"
                '        If (gEos - gEs) > 0.2 Then
                RadnNet = gEos * LAMBDA / gTimeStepSec    ' Eos*L = net Radiation Rn heat flux.
                '        Else
                ' No latent heat and Net Radiation adjustment to d(1)
                'End If
        End Select
        Dim LatentHeatFlux As Double = gEs * LAMBDA / gTimeStepSec      ' Es*L = latent heat flux LE (W/m)
        Dim SoilSurfaceHeatFlux As Double = sensibleHeatFlux + RadnNet - LatentHeatFlux  ' from Rn = G + H + LE (W/m)
        d(SURFACEnode) += SoilSurfaceHeatFlux        ' FIXME JNGH testing alternative net radn

        ' last line is unfulfilled soil water evaporation
        ' The boundary condition at the bottom of the soil column is usually specified as remaining at some constant,
        ' measured temperature, TN(M+1). The last value for D is therefore -

        d(gNz) += nu * gThermalConductance_zb(gNz) * TNew_zb(gNz + 1)
        ' For a no-flux condition, K(M) = 0, so nothing is added.

        ' The Thomas algorithm
        '   Calculate coeffs A, B, C, D for intermediate nodes
        For node As Integer = SURFACEnode To gNz - 1
            c(node) /= b(node)
            d(node) /= b(node)
            b(node + 1) -= a(node + 1) * c(node)
            d(node + 1) -= a(node + 1) * d(node)
        Next node
        TNew_zb(gNz) = d(gNz) / b(gNz)  ' do temperature at bottom node

        ' Do temperatures at intermediate nodes from second bottom to top in soil profile
        For node As Integer = gNz - 1 To SURFACEnode Step -1
            TNew_zb(node) = d(node) - c(node) * TNew_zb(node + 1)
            Call BoundCheck(TNew_zb(node), -50.0, 100.0, "TNew_zb(" + node.ToString() + ")")
        Next node
    End Sub 'doThomas

    ''' <summary>
    '''Numerical solution of the differential equations. Solves the
    '''tri_diagonal matrix using the Thomas algorithm, Thomas, L.H. (1946)
    '''"Elliptic problems in linear difference equations over a network"
    '''Watson Sci Comput. Lab. Report., (Columbia University, New York)"
    ''' RETURNS TNew_zb()
    ''' </summary>
    ''' <remarks>Val Snow's original version</remarks>
    Private Sub doThomas_VS(ByRef TNew_zb() As Double)

        Dim a(gNz + 1) As Double    'A;
        Dim b(gNz) As Double        'B;
        Dim c(gNz) As Double        'C;
        Dim d(gNz) As Double        'D;
        Dim heat(gNz) As Double     'CP; heat storage between nodes - index is same as upper node
        Dim Therm_zb(gNz) As Double 'K; conductance between nodes - index is same as upper node
        ' nu = F; Nz = M; 1-nu = G; T_zb = T; TNew_zb = TN; 

        Therm_zb(0) = gThermConductivity_zb(0)
        For node As Integer = 1 To gNz
            heat(node) = gVolSpecHeatSoil(node) * 0.5 * (gZ_zb(node + 1) - gZ_zb(node - 1)) / gDt
            'rate of heat
            'convert to thermal conduc
            Therm_zb(node) = gThermConductivity_zb(node) / (gZ_zb(node + 1) - gZ_zb(node))
        Next node

        'My version
        a(1) = 0
        b(1) = nu * Therm_zb(1) + nu * Therm_zb(0) + heat(1)
        c(1) = (-nu) * Therm_zb(1)
        d(1) = gT_zb(0) * (1 - nu) * Therm_zb(0) - gT_zb(1) * (1 - nu) * Therm_zb(1) - gT_zb(1) * (1 - nu) * Therm_zb(0) + gT_zb(1) * heat(1) + gT_zb(2) * (1 - nu) * Therm_zb(1) + Therm_zb(0) * TNew_zb(0) * nu

        If (gEos - gEs) > 0.2 Then
            d(1) += (gEos - gEs) * LAMBDA / gTimeStepSec
        Else
            ' No latent heat to add into d(1)
        End If

        ' last line is unfullfilled soil water evaporation
        ' the main loop
        For i As Integer = 2 To gNz - 1
            a(i) = (-nu) * Therm_zb(i - 1)
            b(i) = nu * Therm_zb(i) + nu * Therm_zb(i - 1) + heat(i)
            c(i) = (-nu) * Therm_zb(i)
            d(i) = gT_zb(i - 1) * (1 - nu) * Therm_zb(i - 1) - gT_zb(i) * (1 - nu) * Therm_zb(i) - gT_zb(i) * (1 - nu) * Therm_zb(i - 1) + gT_zb(i) * heat(i) + gT_zb(i + 1) * (1 - nu) * Therm_zb(i)
        Next i

        'lower node
        a(gNz) = (-nu) * Therm_zb(gNz - 1)
        a(gNz + 1) = (-nu) * Therm_zb(gNz)
        b(gNz) = nu * Therm_zb(gNz) + nu * Therm_zb(gNz - 1) + heat(gNz)
        c(gNz) = 0.0R
        c(gNz) = (-nu) * Therm_zb(gNz)
        d(gNz) = gT_zb(gNz - 1) * (1 - nu) * Therm_zb(gNz - 1) - gT_zb(gNz) * (1 - nu) * Therm_zb(gNz) - gT_zb(gNz) * (1 - nu) * Therm_zb(gNz - 1) + gT_zb(gNz) * heat(gNz) + gT_zb(gNz + 1) * (1 - nu) * Therm_zb(gNz) + Therm_zb(gNz) * nu * TNew_zb(gNz + 1)

        'the Thomas algorithm
        For node As Integer = 1 To gNz - 1
            c(node) /= b(node)
            d(node) /= b(node)
            b(node + 1) -= a(node + 1) * c(node)
            d(node + 1) -= a(node + 1) * d(node)
        Next node
        TNew_zb(gNz) = d(gNz) / b(gNz)

        For node As Integer = gNz - 1 To 1 Step -1
            TNew_zb(node) = d(node) - c(node) * TNew_zb(node + 1)
        Next node
    End Sub 'doThomas_VS

    ''' <summary>
    '''  Interpolate air temperature
    ''' </summary>
    ''' <param name="timeHours">time of day that air temperature is required</param>
    ''' <returns>Interpolated air temperature for specified time of day (oC)</returns>
    ''' <remarks>
    ''' Notes:
    '''  Between midinight and MinT_time just a linear interpolation between
    '''  yesterday's midnight temperature and today's MinTg. For the rest of
    '''  the day use a sin function. 
    ''' Note: This can result in the Midnight temperature being lower than the following minimum.
    ''' </remarks>
    Private Function InterpTemp(ByVal timeHours As Double) As Double
        'using global vars:
        'double MaxT_time
        'double MinTg
        'double MaxTg
        'double MinT_yesterday
        'double MaxT_yesterday

        Dim time As Double = timeHours / DAYhrs           ' Current time of day as a fraction of a day
        Dim maxT_time As Double = gMaxT_time / DAYhrs     ' Time of maximum temperature as a fraction of a day
        Dim minT_time As Double = maxT_time - 0.5       ' Time of minimum temperature as a fraction of a day

        If time < minT_time Then
            ' Current time of day is between midnight and time of minimum temperature
            Dim midnightT As Double = Math.Sin((0.0R + 0.25 - maxT_time) * 2.0R * Math.PI) _
                                    * (gMaxTyesterday - gMinTyesterday) / 2.0R _
                                    + (gMaxTyesterday + gMinTyesterday) / 2.0R
            Dim tScale As Double = (minT_time - time) / minT_time

            'set bounds for t_scale (0 <= tScale <= 1)
            If tScale > 1.0R Then
                tScale = 1.0R
            ElseIf tScale < 0 Then
                tScale = 0
            Else
                ' Scaling is ok between 0 and 1.
            End If

            Dim CurrentTemp As Double = gMinT + tScale * (midnightT - gMinT)
            Return CurrentTemp
        Else
            ' Current time of day is at time of minimum temperature or after it up to midnight.
            Dim CurrentTemp As Double = Math.Sin((time + 0.25 - maxT_time) * 2.0R * Math.PI) _
                                      * (gMaxT - gMinT) / 2.0R _
                                      + (gMaxT + gMinT) / 2.0R
            Return CurrentTemp
        End If
    End Function 'InterpTemp

    ''' <summary>
    ''' Determine min, max, and average soil temperature from the
    ''' half-hourly iterations.
    ''' RETURNS gAveTsoil(); gMaxTsoil(); gMinTsoil()
    ''' </summary>
    ''' <param name="IterationsPerDay">number of times in a day the function is called</param>
    ''' <remarks></remarks>
    Private Sub doUpdate(ByVal IterationsPerDay As Integer)

        ' Now transfer to old temperature array
        gTNew_zb.CopyTo(gT_zb, 0)

        ' initialise the min & max to soil temperature if this is the first iteration
        If gTimeOfDaySecs < gDt * 1.2 Then
            For node As Integer = SURFACEnode To gNz
                gMinTsoil(node) = gT_zb(node)
                gMaxTsoil(node) = gT_zb(node)
            Next node
        Else
            ' Do nothing as not first iteration and already initialised
        End If

        For node As Integer = SURFACEnode To gNz
            If gT_zb(node) < gMinTsoil(node) Then
                gMinTsoil(node) = gT_zb(node)
            ElseIf gT_zb(node) > gMaxTsoil(node) Then
                gMaxTsoil(node) = gT_zb(node)
            End If
            gAveTsoil(node) += gT_zb(node) / CDbl(IterationsPerDay)
        Next node
        gBoundaryLayerConductance += gThermConductivity_zb(AIRnode) / CDbl(IterationsPerDay)
    End Sub 'doUpdate
    ''' <summary>
    '''     calculate the density of air (kg/m3) at a given temperature and pressure
    ''' </summary>
    ''' <param name="temperature">temperature (oC)</param>
    ''' <param name="AirPressure">air pressure (hPa)</param>
    ''' <returns>density of air</returns>
    ''' <remarks></remarks>
    Private Function RhoA(ByVal temperature As Double, ByVal AirPressure As Double) As Double

        Const MWair As Double = 0.02897     ' molecular weight air (kg/mol)
        Const RGAS As Double = 8.3143       ' universal gas constant (J/mol/K)
        Const HPA2PA As Double = 100.0      ' hectoPascals to Pascals

        Return divide(MWair * AirPressure * HPA2PA _
                     , kelvinT(temperature) * RGAS _
                     , 0.0)
    End Function
    ''' <summary>
    '''     Calculate atmospheric boundary layer conductance.
    '''     From Program 12.2, p140, Campbell, Soil Physics with Basic.
    ''' </summary>
    ''' <returns>thermal conductivity of surface layer (W/m2/K)</returns>
    ''' <remarks> During first stage drying, evaporation prevents the surface from becoming hot,
    ''' so stability corrections are small. Once the surface dries and becomes hot, boundary layer
    ''' resistance is relatively unimportant in determining evaporation rate.
    ''' A dry soil surface reaches temperatures well above air temperatures during the day, and can be well
    ''' below air temperature on a clear night. Thermal stratification on a clear night can be strong enough
    ''' to reduce sensible heat exchange between the soil surface and the air to almost nothing. If stability
    ''' corrections are not made, soil temperature profiles can have large errors.
    ''' </remarks>
    Private Function boundaryLayerConductance(ByVal TNew_zb() As Double) As Double

        Const VONK As Double = 0.41                 ' VK; von Karman's constant
        Const GRAVITATIONALconst As Double = 9.8    ' GR; gravitational constant (m/s/s)
        Const CAPP As Double = 1010.0               ' (J/kg/K) Specific heat of air at constant pressure
        Const EMISSIVITYsurface As Double = 0.98
        Dim SpecificHeatAir As Double = CAPP * RhoA(gAirT, gAirPressure) ' CH; volumetric specific heat of air (J/m3/K) (1200 at 200C at sea level)
        ' canopy_height, instrum_ht (Z) = 1.2m, AirPressure = 1010
        ' gTNew_zb = TN; gAirT = TA; 

        ' Zero plane displacement and roughness parameters depend on the height, density and shape of
        ' surface roughness elements. For typical crop surfaces, the following empirical correlations have
        ' been obtained. (Extract from Campbell p138.). Canopy height is the height of the roughness elements.
        Dim RoughnessFacMomentum As Double = 0.13 * gCanopyHeight    ' ZM; surface roughness factor for momentum
        Dim RoughnessFacHeat As Double = 0.2 * RoughnessFacMomentum  ' ZH; surface roughness factor for heat
        Dim d As Double = 0.77 * gCanopyHeight                       ' D; zero plane displacement for the surface

        Dim SurfaceTemperature As Double = TNew_zb(SURFACEnode)    ' surface temperature (oC)

        ' To calculate the radiative conductance term of the boundary layer conductance, we need to account for canopy and residue cover
        ' Calculate a diffuce penetration constant (KL Bristow, 1988. Aust. J. Soil Res, 26, 269-80. The Role of Mulch and its Architecture 
        ' in modifying soil temperature). Here we estimate this using the Soilwat algorithm for calculating EOS from EO and the cover effects,
        ' assuming the cover effects on EO are similar to Bristow's diffuse penetration constant - 0.26 for horizontal mulch treatment and 0.44 
        ' for vertical mulch treatment.
        Dim PenetrationConstant As Double = gEos / gEo

        ' Campbell, p136, indicates the radiative conductance is added to the boundary layer conductance to form a combined conductance for
        ' heat transfer in the atmospheric boundary layer. Eqn 12.9 modified for residue and plant canopy cover
        Dim radiativeConductance As Double = 4.0 * STEFAN_BOLTZMANNconst * EMISSIVITYsurface * PenetrationConstant _
                                           * Math.Pow(kelvinT(gAirT), 3)    ' Campbell uses air temperature in leiu of surface temperature

        ' Zero iteration variables
        Dim FrictionVelocity As Double = 0.0        ' FV; UStar
        Dim BoundaryLayerCond As Double = 0.0       ' KH; sensible heat flux in the boundary layer;(OUTPUT) thermal conductivity  (W/m2/K)
        Dim StabilityParam As Double = 0.0          ' SP; Index of the relative importance of thermal and mechanical turbulence in boundary layer transport.
        Dim StabilityCorMomentum As Double = 0.0    ' PM; stability correction for momentum
        Dim StabilityCorHeat As Double = 0.0        ' PH; stability correction for heat
        Dim HeatFluxDensity As Double = 0.0         ' H; sensible heat flux in the boundary layer

        ' Since the boundary layer conductance is a function of the heat flux density, an iterative metnod must be used to find the boundary layer conductance.
        For iteration As Integer = 1 To 3
            ' Heat and water vapour are transported by eddies in the turbulent atmosphere above the crop.
            ' Boundary layer conductance would therefore be expected to vary depending on the wind speed and level
            ' of turbulence above the crop. The level of turbulence, in turn, is determined by the roughness of the surface,
            ' the distance from the surface and the thermal stratification of the boundary layer.
            ' Eqn 12.11 Campbell 
            FrictionVelocity = gWindSpeed * VONK _
                             / (Math.Log((gInstrumHt - d + RoughnessFacMomentum) / RoughnessFacMomentum) _
                               + StabilityCorMomentum)
            ' Eqn 12.10 Campbell
            BoundaryLayerCond = SpecificHeatAir * VONK * FrictionVelocity _
                              / (Math.Log((gInstrumHt - d + RoughnessFacHeat) / RoughnessFacHeat) _
                                + StabilityCorHeat)

            BoundaryLayerCond += radiativeConductance '* (1.0 - sunAngleAdjust())

            HeatFluxDensity = BoundaryLayerCond * (SurfaceTemperature - gAirT)
            ' Eqn 12.14
            StabilityParam = -VONK * gInstrumHt * GRAVITATIONALconst * HeatFluxDensity _
                            / (SpecificHeatAir * kelvinT(gAirT) * Math.Pow(FrictionVelocity, 3.0))

            ' The stability correction parameters correct the boundary layer conductance for the effects
            ' of buoyancy in the atmosphere. When the air near the surface is hotter than the air above, 
            ' the atmosphere becomes unstable, and mixing at a given wind speed is greater than would occur 
            ' in a neutral atmosphere. If the air near the surface is colder than the air above, the atmosphere
            ' is unstable and mixing is supressed.

            If StabilityParam > 0.0 Then
                ' Stable conditions, when surface temperature is lower than air temperature, the sensible heat flux
                ' in the boundary layer is negative and stability parameter is positive.
                ' Eqn 12.15
                StabilityCorHeat = 4.7 * StabilityParam
                StabilityCorMomentum = StabilityCorHeat
            Else
                ' Unstable conditions, when surface temperature is higher than air temperature, sensible heat flux in the
                ' boundary layer is positive and stability parameter is negative.
                StabilityCorHeat = -2.0 * Math.Log((1.0 + Math.Sqrt(1.0 - 16.0 * StabilityParam)) / 2.0)    ' Eqn 12.16
                StabilityCorMomentum = 0.6 * StabilityCorHeat                ' Eqn 12.17

            End If
        Next iteration
        Return BoundaryLayerCond   ' thermal conductivity  (W/m2/K)
    End Function
    ''' <summary>
    '''     Calculate boundary layer conductance.
    '''     From Program 12.2, p140, Campbell, Soil Physics with Basic.
    ''' </summary>
    ''' <returns>thermal conductivity  (W/m2/K)</returns>
    ''' <remarks></remarks>
    Private Function boundaryLayerConductanceConst() As Double

        ' canopy_height, instrum_ht = 1.2m, AirPressure = 1010
        ' therm_cond(0) = 20.0 ' boundary layer conductance W m-2 K-1

        Return boundary_layer_cond '(W/m2/K)
    End Function
    ''' <summary>
    ''' Convert deg Celcius to deg Kelvin
    ''' </summary>
    ''' <param name="celciusT">(INPUT) Temperature in deg Celcius</param>
    ''' <returns>Temperature in deg Kelvin</returns>
    ''' <remarks></remarks>
    Private Function kelvinT(ByVal celciusT As Double) As Double
        Const ZEROTkelvin As Double = 273.18   'Absolute Temperature (oK)
        Return celciusT + ZEROTkelvin '(deg K)
    End Function
    Private Function longWaveRadn(ByVal emissivity As Double, ByVal tDegC As Double) As Double
        Return STEFAN_BOLTZMANNconst * emissivity * Math.Pow(kelvinT(tDegC), 4)
    End Function
#End Region

#Region "SoilN initial soilTemp"
    ''' <summary>
    '''  Purpose
    '''           Calculates average soil temperature at the centre of each layer
    '''           based on the soil temperature model of EPIC (Williams et al 1984)
    ''' </summary>
    ''' <param name="soilTempIO">(OUTPUT) temperature of each layer in profile</param>
    ''' <remarks></remarks>
    Private Sub soiln2_soil_temp(ByRef soilTempIO() As Double)

        Const SUMMER_SOLSTICE_NTH As Double = 173.0        ' day of year of nth'rn summer solstice
        Const TEMPERATURE_DELAY As Double = 27.0    ' delay from solstice to warmest day (days)
        Const HOTTEST_DAY_NTH As Double = SUMMER_SOLSTICE_NTH + TEMPERATURE_DELAY         ' warmest day of year of nth hemisphere
        Const SUMMER_SOLSTICE_STH As Double = SUMMER_SOLSTICE_NTH + DAYSinYear / 2.0     ' day of year of sthrn summer solstice
        Const HOTTEST_DAY_STH As Double = SUMMER_SOLSTICE_STH + TEMPERATURE_DELAY ' warmest day of year of sth hemisphere
        Const ANG As Double = DOY2RAD ' length of one day in radians factor to convert day of year to radian fraction of year

        Dim soil_temp(gNz + 1) As Double
        Array.ConstrainedCopy(soilTempIO, SURFACEnode, soil_temp, 0, gNz)

        ' Get a factor to calculate "normal" soil temperature from the
        ' day of g_year assumed to have the warmest average soil temperature
        ' of the g_year.  The normal soil temperature varies as a cosine
        ' function of alx.  This is the number of radians (time) of a
        ' g_year today is from the warmest soil temp.

        Dim alx As Double = 0.0                ' time in radians of year from hottest
        '  instance to current day of year as a
        '  radian fraction of one year for soil
        '  temperature calculations

        ' check for nth/sth hemisphere
        If (latitude > 0.0) Then
            alx = ANG * (offset_day_of_year(year, day, CInt(-HOTTEST_DAY_NTH)))
        Else
            alx = ANG * (offset_day_of_year(year, day, CInt(-HOTTEST_DAY_STH)))
        End If

        Call BoundCheck(alx, 0.0, 6.31, "alx")

        ' get change in soil temperature since hottest day. deg c.

        Dim dlt_temp As Double = soiln2_soiltemp_dt(alx)    ' change in soil temperature

        ' get temperature damping depth. (mm per radian of a g_year)

        Dim damp As Double = soiln2_soiltemp_dampdepth()    ' temperature damping depth (mm depth/radians time)

        Dim cum_depth As Double = 0.0       ' cumulative depth in profile

        ' Now get the average soil temperature for each layer.
        ' The difference in temperature between surface and subsurface
        ' layers ( exp(zd)) is an exponential function of the ratio of
        ' the depth to the bottom of the layer and the temperature
        ' damping depth of the soil.

        Dim depth_lag As Double = 0.0             ' temperature lag factor in radians for depth

        Call SetArray(soil_temp, 0.0)
        For layer As Integer = 1 To gNumLayers

            ' get the cumulative depth to bottom of current layer

            cum_depth = cum_depth + gDLayer(layer)

            ' get the lag factor for depth. This reduces changes in
            ' soil temperature with depth. (radians of a g_year)

            depth_lag = divide(cum_depth, damp, 0.0)

            ' allow subsurface temperature changes to lag behind
            ' surface temperature changes

            soil_temp(layer) = soiln2_layer_temp(depth_lag, alx, dlt_temp)
            Call BoundCheck(soil_temp(layer), -20.0, 80.0, "soil_temp")
        Next layer
        Array.ConstrainedCopy(soil_temp, 0, soilTempIO, SURFACEnode, gNz)
    End Sub
    ''' <summary>
    ''' 
    ''' </summary>
    ''' <param name="depth_lag">(INPUT) lag factor for depth (radians)</param>
    ''' <param name="alx">(INPUT) time in radians of a g_year from hottest instance</param>
    ''' <param name="dlt_temp">(INPUT) change in surface soil temperature since hottest day (deg c)</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Private Function soiln2_layer_temp(ByVal depth_lag As Double, ByVal alx As Double, ByVal dlt_temp As Double) As Double
        ' Now get the average soil temperature for the layer.
        ' The difference in temperature between surface and subsurface
        ' layers ( exp(-depth_lag)) is an exponential function of the ratio of
        ' the depth to the bottom of the layer and the temperature
        ' damping depth of the soil.

        Return tav + (amp / 2.0 * Math.Cos(alx - depth_lag) + dlt_temp) * Math.Exp(-depth_lag)
    End Function
    ''' <summary>
    '''  Purpose
    '''           Calculates  the rate of change in soil surface temperature
    '''           with time.
    ''' </summary>
    ''' <param name="alx">(INPUT) time of year in radians from warmest instance</param>
    ''' <returns>Change in temperature</returns>
    ''' <remarks>
    '''           jngh 24-12-91.  I think this is actually a correction to adjust
    '''           today's normal sinusoidal soil surface temperature to the
    '''           current temperature conditions.
    ''' </remarks>
    Private Function soiln2_soiltemp_dt(ByVal alx As Double) As Double
        ' Get today's top layer temp from yesterdays temp and today's
        ' weather conditions.
        ' The actual soil surface temperature is affected by current
        ' weather conditions.

        ' Get today's normal surface soil temperature
        ' There is no depth lag, being the surface, and there
        ' is no adjustment for the current temperature conditions
        ' as we want the "normal" sinusoidal temperature for this
        ' time of year.

        Dim temp_a As Double = soiln2_layer_temp(0.0, alx, 0.0)

        ' Get the rate of change in soil surface temperature with time.
        ' This is the difference between a five-day moving average and
        ' today's normal surface soil temperature.

        Dim dT As Double = SurfaceTemperatureInit() - temp_a

        ' check output
        Call BoundCheck(dT, -100.0, 100.0, "Initial SoilTemp_dt")

        Return dT
    End Function
    ''' <summary>
    ''' Calculate initial soil surface temperature
    ''' </summary>
    ''' <returns> initial soil surface temperature</returns>
    ''' <remarks></remarks>
    Private Function SurfaceTemperatureInit() As Double
        Dim ave_temp As Double = (gMaxT + gMinT) * 0.5
        Dim surfaceT As Double = (1.0 - salb) * (ave_temp + (gMaxT - ave_temp) * Math.Sqrt(radn * 23.8846 / 800.0)) + salb * ave_temp
        Call BoundCheck(surfaceT, -100.0, 100.0, "Initial surfaceT")
        Return surfaceT
    End Function
    ''' <summary>
    ''' Purpose
    '''           Now get the temperature damping depth. This is a function of the
    '''             average bulk density of the soil and the amount of water above
    '''             the lower limit. I think the damping depth units are
    '''             mm depth/radian of a year
    ''' </summary>
    ''' <returns>soil temperature damping depth (mm)</returns>
    ''' <remarks>
    '''      Notes
    '''       241091 consulted Brian Wall.  For soil temperature an estimate of
    '''       the water content of the total profile is required, not the plant
    '''       extractable soil water.  Hence the method used here - difference
    '''       total lower limit and total soil water instead of sum of differences
    '''       constrained to and above.  Here the use of lower limit is of no
    '''       significance - it is merely a reference point, just as 0.0 could
    '''       have been used.  jngh
    ''' </remarks>
    Private Function soiln2_soiltemp_dampdepth() As Double

        Const SW_AVAIL_TOT_MIN As Double = 0.01   ' minimum available sw (mm water)

        Dim ave_bd As Double = 0.0                ' average bulk density over layers (g/cc soil)
        Dim sw_avail_tot As Double = 0.0          ' amount of sw above lower limit (mm water)
        Dim b As Double = 0.0                     ' intermediate variable
        Dim cum_depth As Double = 0.0             ' cumulative depth in profile (mm)
        Dim damp_depth_max As Double = 0.0        ' maximum damping depth (potential)(mm soil/radian of a g_year (58 days))
        Dim f As Double = 0.0                     ' fraction of potential damping depth discounted by water content of soil (0-1)
        Dim favbd As Double = 0.0                 ' a function of average bulk density
        Dim wcf As Double = 0.0                   ' a function of water content (0-1)
        Dim bd_tot As Double = 0.0                ' total bulk density over profile (g/cc soil)
        Dim ll_tot As Double = 0.0                ' total lower limit over profile (mm water)
        Dim sw_dep_tot As Double = 0.0            ' total soil water over profile (mm water)
        Dim wc As Double = 0.0                    ' water content of profile (0-1)
        Dim ww As Double = 0.0                    ' potential sw above lower limit (mm water/mm soil)

        '- Implementation Section ----------------------------------

        ' get average bulk density

        bd_tot = sum_products_real_array(gRhob, gDLayer)
        cum_depth = SumOfRange(gDLayer, 1, gNumLayers)
        ave_bd = divide(bd_tot, cum_depth, 0.0)

        ' favbd ranges from almost 0 to almost 1
        ' damp_depth_max ranges from 1000 to almost 3500
        ' It seems damp_depth_max is the damping depth potential.

        favbd = divide(ave_bd, (ave_bd + 686.0 * Math.Exp(-5.63 * ave_bd)), 0.0)
        damp_depth_max = 1000.0 + 2500.0 * favbd
        damp_depth_max = l_bound(damp_depth_max, 0.0)

        ' Potential sw above lower limit - mm water/mm soil depth
        ' note that this function says that average bulk density
        ' can't go above 2.47222, otherwise potential becomes negative.
        ' This function allows potential (ww) to go from 0 to .356

        ww = 0.356 - 0.144 * ave_bd
        ww = l_bound(ww, 0.0)


        ' calculate amount of soil water, using lower limit as the
        ' reference point.

        ll_tot = SumOfRange(ll15_dep, 0, gNumLayers - 1)
        sw_dep_tot = SumOfRange(sw_dep, 0, gNumLayers - 1)
        sw_avail_tot = sw_dep_tot - ll_tot
        sw_avail_tot = l_bound(sw_avail_tot, SW_AVAIL_TOT_MIN)

        ' get fractional water content -

        ' wc can range from 0 to 1 while
        ' wcf ranges from 1 to 0

        wc = divide(sw_avail_tot, (ww * cum_depth), 1.0)
        wc = bound(wc, 0.0, 1.0)
        wcf = divide((1.0 - wc), (1.0 + wc), 0.0)

        ' Here b can range from -.69314 to -1.94575
        ' and f ranges from 1 to  0.142878
        ' When wc is 0, wcf=1 and f=500/damp_depth_max
        ' and soiln2_SoilTemp_DampDepth=500
        ' When wc is 1, wcf=0 and f=1
        ' and soiln2_SoilTemp_DampDepth=damp_depth_max
        ' and that damp_depth_max is the maximum.


        b = Math.Log(divide(500.0, damp_depth_max, 10000000000.0))

        f = Math.Exp(b * Math.Pow(wcf, 2))

        ' Get the temperature damping depth. (mm soil/radian of a g_year)
        ' discount the potential damping depth by the soil water deficit.
        ' Here soiln2_SoilTemp_DampDepth ranges from 500 to almost
        ' 3500 mm/58 days.

        Return f * damp_depth_max
    End Function

#End Region
#Region "Test Net radiation alternative"
    ''' <summary>
    ''' Calculate initial variables for net radiation per timestep
    ''' </summary>
    ''' <param name="solarRadn">(OUTPUT)</param>
    ''' <param name="cloudFr">(OUTPUT)</param>
    ''' <param name="cva">(OUTPUT)</param>
    ''' <param name="ITERATIONSperDAY"> (INPUT)</param>
    ''' <remarks></remarks>
    Private Sub doNetRadiation(ByRef solarRadn() As Double, ByRef cloudFr As Double, ByRef cva As Double, ByVal ITERATIONSperDAY As Integer)
        Dim TSTEPS2RAD As Double = DEG2RAD * 360.0 / CDbl(ITERATIONSperDAY)          ' convert timestep of day to radians
        Const SOLARconst As Double = 1360.0     ' W/M^2
        Dim solarDeclination As Double = 0.3985 * Math.Sin(4.869 + day * DOY2RAD + 0.03345 * Math.Sin(6.224 + day * DOY2RAD))
        Dim cD As Double = Math.Sqrt(1.0 - solarDeclination * solarDeclination)
        Dim m1(ITERATIONSperDAY) As Double
        Dim m1Tot As Double = 0.0
        For timestepNumber As Integer = 1 To ITERATIONSperDAY
            m1(timestepNumber) = (solarDeclination * Math.Sin(latitude * DEG2RAD) + cD * Math.Cos(latitude * DEG2RAD) * Math.Cos(TSTEPS2RAD * (CDbl(timestepNumber) - CDbl(ITERATIONSperDAY) / 2.0))) * 24.0 / CDbl(ITERATIONSperDAY)
            If m1(timestepNumber) > 0.0 Then
                m1Tot += m1(timestepNumber)
            Else
                m1(timestepNumber) = 0.0
            End If
        Next timestepNumber

        Const W2MJ As Double = HR2MIN * MIN2SEC * J2MJ      ' convert W to MJ
        Dim psr As Double = m1Tot * SOLARconst * W2MJ   ' potential solar radiation for the day (MJ/m^2)
        Dim fr As Double = radn / psr               ' ratio of potential to measured daily solar radiation (0-1)
        cloudFr = 2.33 - 3.33 * fr    ' fractional cloud cover (0-1)
        cloudFr = bound(cloudFr, 0.0, 1.0)

        For timestepNumber As Integer = 1 To ITERATIONSperDAY
            solarRadn(timestepNumber) = radn * m1(timestepNumber) / m1Tot
        Next timestepNumber

        ' cva is vapour concentration of the air (g/m^3)
        cva = Math.Exp(31.3716 - 6014.79 / kelvinT(gMinT) - 0.00792495 * kelvinT(gMinT)) / kelvinT(gMinT)
    End Sub
    ''' <summary>
    ''' Calculate the net radiation at the soil surface.
    ''' </summary>
    ''' <param name="solarRadn"></param>
    ''' <param name="cloudFr"></param>
    ''' <param name="cva"></param>
    ''' <returns>Net radiation (SW and LW) for timestep (MJ)</returns>
    ''' <remarks></remarks>
    Private Function radnNetInterpolate(ByVal solarRadn As Double, ByVal cloudFr As Double, ByVal cva As Double) As Double
        Const EMISSIVITYsurface As Double = 0.96    ' Campbell Eqn. 12.1
        Dim w2MJ As Double = gDt * J2MJ      ' convert W to MJ

        ' Eqns 12.2 & 12.3
        Dim emissivityAtmos As Double = (1 - 0.84 * cloudFr) * 0.58 * Math.Pow(cva, (1.0 / 7.0)) + 0.84 * cloudFr
        ' To calculate the longwave radiation out, we need to account for canopy and residue cover
        ' Calculate a penetration constant. Here we estimate this using the Soilwat algorithm for calculating EOS from EO and the cover effects.
        Dim PenetrationConstant As Double = gEos / gEo

        ' Eqn 12.1 modified by cover.
        Dim lwRinSoil As Double = longWaveRadn(emissivityAtmos, gAirT) * PenetrationConstant * w2MJ

        Dim lwRoutSoil As Double = longWaveRadn(EMISSIVITYsurface, gT_zb(SURFACEnode)) * PenetrationConstant * w2MJ '_
        '                        + longWaveRadn(emissivityAtmos, (gT_zb(SURFACEnode) + gAirT) * 0.5) * (1.0 - PenetrationConstant) * w2MJ

        ' Ignore (mulch/canopy) temperature and heat balance
        Dim lwRnetSoil As Double = lwRinSoil - lwRoutSoil

        Dim swRin As Double = solarRadn
        Dim swRout As Double = salb * solarRadn
        'Dim swRout As Double = (salb + (1.0 - salb) * (1.0 - sunAngleAdjust())) * solarRadn   'FIXME temp test
        Dim swRnetSoil As Double = (swRin - swRout) * PenetrationConstant
        Return swRnetSoil + lwRnetSoil
    End Function
    Private Function sunAngleAdjust() As Double
        Dim solarDeclination As Double = 0.3985 * Math.Sin(4.869 + day * DOY2RAD + 0.03345 * Math.Sin(6.224 + day * DOY2RAD))
        Dim zenithAngle As Double = Math.Abs(latitude - solarDeclination * RAD2DEG)
        Dim sunAngle As Double = 90.0 - zenithAngle
        Dim fr As Double = sunAngle / 90.0
        Return bound(fr, 0.0, 1.0)
    End Function
#End Region

#Region "Utilities"
    ''' <summary>
    '''       Divides one number by another.  If the divisor is zero or overflow
    '''       would occur a specified default is returned.  If underflow would
    '''       occur, nought is returned.
    ''' </summary>
    ''' <param name="dividend">dividend - quantity to be divided</param>
    ''' <param name="divisor">divisor</param>
    ''' <param name="defaultValue">default value if overflow, underflow or divide by zero</param>
    ''' <returns></returns>
    ''' <remarks>
    '''  Definition
    '''     Returns (dividend / divisor) if the division can be done
    '''     without overflow or underflow.  If divisor is zero or
    '''     overflow would have occurred, default is returned.  If
    '''     underflow would have occurred, zero is returned.
    '''
    ''' Assumptions
    '''       largest/smallest real number is 1.0e+/-30
    ''' </remarks>
    Private Function divide(ByVal dividend As Object, ByVal divisor As Object, ByVal defaultValue As Object) As Object

        Const LARGEST As Double = 1.0E+30   ' largest acceptable no. for quotient
        Const NOUGHT As Double = 0.0      ' 0
        Const SMALLEST As Double = 1.0E-30    ' smallest acceptable no. for quotient

        '+ Local Variables
        Dim quotient As Double = 0.0              ' quotient

        If (RealsAreEqual(dividend, NOUGHT)) Then   ' multiplying by 0
            quotient = NOUGHT

        ElseIf (RealsAreEqual(divisor, NOUGHT)) Then ' dividing by 0
            quotient = defaultValue

        ElseIf (Math.Abs(divisor) < 1.0) Then          ' possible overflow
            If (Math.Abs(dividend) > Math.Abs(LARGEST * divisor)) Then     ' overflow
                quotient = defaultValue
            Else                               ' ok
                quotient = dividend / divisor
            End If

        ElseIf (Math.Abs(divisor) > 1.0) Then          ' possible underflow
            If (Math.Abs(dividend) < Math.Abs(SMALLEST * divisor)) Then     ' underflow
                quotient = NOUGHT
            Else                               ' ok
                quotient = dividend / divisor
            End If

        Else                                  ' ok
            quotient = dividend / divisor
        End If

        Return quotient
    End Function
    ''' <summary>
    ''' Get the sum of all elements in an array between 'start' and 'end'
    ''' </summary>
    ''' <param name="array"></param>
    ''' <param name="start"></param>
    ''' <param name="end"></param>
    ''' <returns></returns>
    Private Function SumOfRange(ByVal array() As Double, ByVal start As Integer, ByVal [end] As Integer) As Double
        Dim result As Double = 0
        For i As Integer = start To [end]
            result += array(i)
        Next i
        Return result
    End Function

    ''' <summary>
    ''' Used for dbugging purposes
    ''' </summary>
    ''' <param name="dlayer"></param>
    ''' <returns></returns>
    Private Function dumpArray(ByVal DLayer As Double()) As String
        Dim result As String = String.Empty
        For Each d As Double In DLayer
            result += d.ToString() & vbTab
        Next d
        Return result
    End Function

    ''' <summary>
    '''     checks if a variable lies outside lower and upper bounds.
    '''     Reports an err if it does.
    ''' </summary>
    ''' <param name="VariableValue">value to be validated</param>
    ''' <param name="Lower">lower limit of value</param>
    ''' <param name="Upper">upper limit of value</param>
    ''' <param name="VariableName">variable name to be validated</param>
    ''' <remarks>
    '''  Definition
    '''     This subroutine will issue a warning message using the
    '''     name of "value", "vname", if "value" is greater than
    '''     ("upper" + 2 * error_margin("upper")) or if "value" is less than
    '''     ("lower" - 2 *error_margin("lower")).  If  "lower" is greater
    '''     than ("upper" + 2 * error_margin("upper")) , then a warning
    '''     message will be flagged to that effect.
    '''
    ''' Notes
    '''     reports err if value GT upper or value LT lower or lower GT upper
    ''' </remarks>
    Private Overloads Sub BoundCheck(ByVal VariableValue As Object, ByVal Lower As Object, ByVal Upper As Object, ByVal VariableName As String)

        Const MARGIN As Double = 0.00001          ' margin for precision err of lower
        Dim LowerBound As Double = Lower - MARGIN       ' calculate a margin for precision err of lower.
        Dim UpperBound As Double = Upper + MARGIN   ' calculate a margin for precision err of upper.

        If (LowerBound > UpperBound) Then     ' are the upper and lower bounds valid?
            Throw New Exception("Lower bound (" + Lower.ToString() + ") exceeds upper bound (" + Upper.ToString() + ") in bounds checking: Variable is not checked")

        ElseIf (VariableValue > UpperBound Or VariableValue < LowerBound) Then       ' is the value outside range?
            Throw New Exception(VariableName + " = " + VariableValue.ToString() + " is outside range of " + Lower.ToString() + " to " + Upper.ToString())

            'ElseIf (VariableValue < LowerBound) Then       ' is the value too small?
            '    Throw New Exception(VariableName + " = " + VariableValue.ToString() + " less than lower limit of " + Lower.ToString())
        Else
            'all(ok!)
        End If

        Return
    End Sub
    ''' <summary>
    ''' Check bounds of values in an array
    ''' </summary>
    ''' <param name="array">array to be checked</param>
    ''' <param name="LowerBound">lower bound of values</param>
    ''' <param name="UpperBound">upper bound of values</param>
    ''' <param name="ArrayName">key string of array</param>
    ''' <remarks>
    '''  Definition
    '''     Each of the "size" elements of "array" should be greater than or equal to
    '''     ("lower" - 2 *error_margin("lower")) and less than or equal to
    '''     ("upper" + 2 * error_margin("upper")).  A warning error using
    '''     the name of "array", "name", will be flagged for each element
    '''     of "array" that fails the above test.  If  "lower" is greater
    '''     than ("upper" + 2 * error_margin("upper")) , then a warning
    '''     message will be flagged to that effect "size" times.
    '''
    '''  Assumptions
    '''     each element has same bounds.
    ''' </remarks>
    Private Overloads Sub BoundCheckArray(ByVal array() As Double, ByVal LowerBound As Double, ByVal UpperBound As Double, ByVal ArrayName As String)

        If UBound(array) >= 1 Then
            For index As Integer = LBound(array) To UBound(array)
                Call BoundCheck(array(index), LowerBound, UpperBound, ArrayName)
            Next index
        Else

        End If
    End Sub
    ''' <summary>
    ''' Tests if two real values are practically equal
    ''' </summary>
    ''' <param name="double1"></param>
    ''' <param name="double2"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Function RealsAreEqual(ByVal double1 As Double, ByVal double2 As Double) As Boolean
        Dim precision As Double = Math.Min(double1, double2) * 0.0001
        Return (Math.Abs(double1 - double2) <= precision)
    End Function
    ''' <summary>
    ''' constrains a variable to upper bound of upper
    ''' </summary>
    ''' <param name="var">(INPUT) variable to be constrained</param>
    ''' <param name="upper">(INPUT) upper limit of variable</param>
    ''' <returns></returns>
    ''' <remarks>
    ''' Returns "var" providing that it is less than or equal to the upper bound, "upper".  Otherwise returns "upper".
    ''' </remarks>
    Private Function u_bound(ByVal var As Double, ByVal upper As Double) As Double
        Return Math.Min(var, upper)
    End Function
    ''' <summary>
    ''' constrains a variable to or above lower bound of lower
    ''' </summary>
    ''' <param name="var">(INPUT) variable to be constrained</param>
    ''' <param name="lower">(INPUT) lower limit of variable</param>
    ''' <returns></returns>
    ''' <remarks>
    ''' Returns "var" providing that it is greater than or equal to the lower bound, "lower".  Otherwise returns "lower".
    ''' </remarks>
    Private Function l_bound(ByVal var As Double, ByVal lower As Double) As Double
        Return Math.Max(var, lower)
    End Function
    ''' <summary>
    ''' constrains a variable within bounds of lower and upper
    ''' </summary>
    ''' <param name="var">(INPUT) variable to be constrained</param>
    ''' <param name="lower">(INPUT) lower limit of variable</param>
    ''' <param name="upper">(INPUT) upper limit of variable</param>
    ''' <returns>Constrained value</returns>
    ''' <remarks>
    ''' Returns "lower", if "var" is less than "lower".  Returns "upper" if "var" is greater than "upper".  Otherwise returns "var".  
    ''' A warning error is flagged if "lower" is greater than "upper".
    ''' If the lower bound is > the upper bound, the variable remains unconstrained.
    ''' </remarks>
    Private Function bound(ByVal var As Double, ByVal lower As Double, ByVal upper As Double) As Double
        Dim high As Double = 0.0                  ' temporary variable constrained to upper limit of variable

        ' check that lower & upper bounds are valid

        If (lower > upper) Then
            ' bounds invalid, don't constrain variable
            Throw New Exception("Lower bound (" + lower.ToString() + ") exceeds upper bound (" + upper.ToString() + ") in bounds checking: Variable (value=" + var.ToString() + ") is not constrained between bounds")
            Return var
        Else
            ' bounds valid, now constrain variable
            high = u_bound(var, upper)
            Return l_bound(high, lower)
        End If
    End Function
    ''' <summary>
    '''  sets real array var to value up to level limit
    ''' </summary>
    ''' <param name="var">(OUTPUT) array to set</param>
    ''' <param name="value">(INPUT) value to set array</param>
    ''' <remarks> Sets all elements of "var" to "value".</remarks>
    Private Sub SetArray(ByRef var() As Double, ByVal value As Double)
        For i As Integer = var.GetLowerBound(0) To var.GetUpperBound(0)
            var(i) = value
        Next i
    End Sub
    ''' <summary>
    ''' returns sum_of of products of arrays var1 and var2, up to level limit. 
    ''' each level of one array is multiplied by the corresponding level of the other.
    ''' </summary>
    ''' <param name="var1">(INPUT) first array for multiply</param>
    ''' <param name="var2">(INPUT) 2nd array for multiply</param>
    ''' <returns>Returns sum of  ("var1"(j) * "var2"(j))   for all j in  1 .. upperBound.</returns>
    ''' <remarks></remarks>
    Private Function sum_products_real_array(ByVal var1() As Double, ByVal var2() As Double) As Double
        If (var1.GetUpperBound(0) = var2.GetUpperBound(0)) Then
            Dim tot As Double = 0.0
            For level As Integer = 0 To var1.GetUpperBound(0)
                tot = tot + var1(level) * var2(level)
            Next level

            Return tot
        Else
            Throw New Exception("sum_products_real_array must have same size arrays. Array1 size =" + var1.GetLength(0).ToString() + ". Array2 sixe =" + var2.GetLength(0).ToString())
        End If
    End Function
    ''' <summary>
    ''' Multiplies array by specified multiplier
    ''' </summary>
    ''' <param name="array">(INPUT/OUTPUT)</param>
    ''' <param name="multiplier"></param>
    ''' <remarks></remarks>
    Private Sub multiplyArray(ByVal array() As Double, ByVal multiplier As Double)
        For level As Integer = 0 To array.GetUpperBound(0)
            array(level) = array(level) * multiplier
        Next level
    End Sub
    ''' <summary>
    '''  adds or subtracts specified days to/from day of year number
    ''' </summary>
    ''' <param name="iyr">(INPUT) year</param>
    ''' <param name="doy">(INPUT) day of year number</param>
    ''' <param name="ndays">(INPUT) number of days to adjust by</param>
    ''' <returns>New day of year</returns>
    ''' <remarks> Returns the day of year for the day "ndays" after the day specified by the day of year, "doy", in the year, "iyr".
    '''  "ndays" may well be negative.
    ''' </remarks>
    Private Function offset_day_of_year(ByVal iyr As Integer, ByVal doy As Integer, ByVal ndays As Integer) As Integer
        Dim newdate As DateTime = New DateTime(iyr, 1, 1).AddDays(doy - 1 + ndays)
        Return newdate.DayOfYear
    End Function


#End Region

End Class