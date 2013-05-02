Imports System
Imports System.Collections.Generic
Imports System.Text
Imports ModelFramework

'20110523 - Convert all internals to function on a per hectare basic [issues with scaling somewhere in here


'Bugs:
'    Rotation lenghts not being respected. Need to implement the day count type method or fix paddock status
'    Grazing counter can reach -1

' Why doesn't the summarry file get updated like the console output

' Apsim Types possibly applicable to DDRules
'       AnimalGrazeType
'       AddExcretaType
'       ApplyUrineType
'       DryOffStockType
'       FaecesType
'       GrazeType
'       IntakeType
'       MoveStockType
'       PastureIntake
'       PastureCut [Double cut_height, gathered, dmd_loss, dm_content]
'       PastureOnCutType [Double fresh_wt, dm_content, dm, cp_conc, p_conc, s_conc, ash_alk]
'       SuppEaten
'       SuppIntake
'       SupplementBuyType

' LUDF Grazing rules
'       post 1480 = 7-8 clicks
'       pre 3100-3200
'       average 2300-2400
'       rotation: 20 days during peak growth
'                 out to 35 days at drying off
'       Paddocks over pre-graze taken out and cut immediately

' Silage cutting
'   Only remove surplus e.g. if in surplus by 18TDM only remove 18 TDM at cutting by changing the residual (not realistic, see weekly notes)

' Feed deficit?
' Short 14TDM on wedge == feed 2 TDM/day for the week


Public Class DDRules
    Public myDebugLevel As Integer = 0
    <Link()> Public MySimulation As Simulation
    <Link()> Public MyClock As Clock
    <Input()> Public is_end_week As Boolean
    <Input()> Public is_start_week As Boolean
    <Link()> Public myEffluentPond As New EffluentPond
    <Link()> Public myEffluentIrrigator As New EffluentIrrigator

    Private myFarm As Farm
    Private myHerd As SimpleHerd 'local handle to the herd contained in Farm. Is this only a short term fix?

    'Grazing residual
    Public dairyNZ_gr As Integer() = {1600, 1600, 1600, 1500, 1400, 1200, 1200, 1400, 1500, 1500, 1500, 1500}  'june to may
    Public Val_gr As Integer() = {1600, 1600, 1600, 1600, 1600, 1200, 1200, 1600, 1600, 1600, 1600, 1600}  'june to may - altered by Val for FarmSim

    'Grazing interval
    Private dairyNZ_mg As Integer() = {20, 25, 30, 40, 50, 100, 100, 80, 50, 25, 20, 20}  'jan to dec

    Public default_mg As Integer() = dairyNZ_mg
    Public default_gr As Integer() = dairyNZ_gr

    Dim strEffluentPaddocks() As String = {"10T", "11P", "12P"}
    Dim strLanewayPaddocks() As String = {"Lanes"}
    'Dim myIrrigationAmount As Double = 0
    Dim myFertiliserAmount As Double = 0
    Private TotalFarmArea As Double = 0
    Private GrazingIntervalIsSet As Boolean = False
    Private GrazingResidualIsSet As Boolean = False
    'Dim myIrrigation_efficiency As Double = 1.0

    Private a As Double = 500
    Private b As Double = 140

    Private Rotation As Integer = 21

    Private Live_Weight As Double = 480
    Private ConditionScore As Double = 3
    'Private BC As Double()



    Public Function StringtoIntegerArray(ByVal strArray As String) As Integer()
        Dim strvalues As String() = strArray.Split(",".ToCharArray())
        Dim dblvalues(strvalues.Length - 1) As Integer
        For i As Integer = 0 To strvalues.Length - 1
            dblvalues(i) = CInt(strvalues(i))
        Next
        Return dblvalues
    End Function

    Public Function StringtoDoubleArray(ByVal text As String) As Double()
        Dim stringdoubles() As String = text.Split(",".ToCharArray())
        Dim doubleArray(stringdoubles.Length - 1) As Double
        For i As Integer = 0 To stringdoubles.Length - 1
            doubleArray(i) = Convert.ToDouble(stringdoubles(i))
        Next
        Return doubleArray
    End Function


    Public Sub New()
        myFarm = New Farm()
        DebugLevel = myDebugLevel

    End Sub

#Region "EventHandlers"
    '<EventHandler()> Public Sub OnInit1()
    '        If (DebugLevel > 0) Then
    '                Console.WriteLine("Enter OnInit1()")
    '                For Each pdk As Paddock In MySimulation.SubPaddocks
    '                        Console.WriteLine("   " + pdk.Name)
    '                Next
    '        End If
    'End Sub

    <Description("Initilise farm components")> _
    <EventHandler()> Public Sub OnInit2()
        DebugLevel = myDebugLevel
        If (DebugLevel > 0) Then
            Console.WriteLine("DDRules Entering OnInit2()")
        End If

        ' ************* Farm testing **********************
        If (TotalFarmArea <= 0) Then
            TotalFarmArea = MySimulation.ChildPaddocks.Count 'default to one hectare paddocks if no area set
        End If

        If (DebugLevel > 2) Then
            Console.WriteLine("*** DDRules OnInit2() - Farm.Init")
            If (myFarm IsNot Nothing) Then
                Console.WriteLine("*** DDRules OnInit2() - Farm = " + myFarm.ToString())
            Else
                Console.WriteLine("*** DDRules OnInit2() - Farm = " + "NULL VALUE")
            End If
        End If

        myFarm.Init(MySimulation, MyClock.year, MyClock.month, TotalFarmArea)
        myFarm.setEffluent(myEffluentPond, myEffluentIrrigator) 'This sets myEffluentPond to myEffluentPond, and myEffluentIrrigator to myEffluentIrrigator.  Eh??!
        myFarm.setEffluentPaddocks(strEffluentPaddocks)
        myFarm.setLanewayPaddocks(strLanewayPaddocks)

        If (DebugLevel > 2) Then
            Console.WriteLine("*** DDRules OnInit2() - Get herd reference")
        End If
        myHerd = myFarm.getHerd()
        myHerd.ReferenceCow.Live_Weight = Live_Weight
        myHerd.ReferenceCow.ConditionScore = ConditionScore

        If (DebugLevel > 2) Then
            Console.WriteLine("*** DDRules OnInit2() - Test for FarmSim Components")
        End If
        SetupFarmSim(MySimulation)

        If (DebugLevel > 2) Then
            Console.WriteLine("*** DDRules OnInit2() - Print Summary")
        End If
        PrintFarmSummary()

        GrazingIntervalIsSet = False
        GrazingResidualIsSet = False
        'PaddockGrazable(0) = 0 'testing removal of a paddock from the rotation i.e. for forage crops etc.
        'PaddockGrazable(2) = 0 'testing removal of a paddock from the rotation i.e. for forage crops etc.
        'myIrrigation_efficiency = 1.0

        If (DebugLevel > 2) Then
            Console.WriteLine("DDRules OnInit2() Complete!")
        End If
    End Sub

    <Description("Reset tracked output variables")> _
    <EventHandler()> Sub OnNewMet(ByVal NewMet As NewMetType)
        'myIrrigationAmount = 0
        myFertiliserAmount = 0
    End Sub

    <Description("Reset paddock status etc.")> _
    <EventHandler()> Sub OnPrepare()
        If (DebugLevel > 0) Then
            Console.WriteLine("Enter OnPrepare()")
            Console.WriteLine("        " & MyClock.simulation_days)
        End If

        myFarm.Prepare(MyClock.year, MyClock.month, MyClock.day_of_month, is_end_week)
        'myFarm.StockingRate = BaseStockingRate

        If (DebugLevel > 0) Then
            Console.WriteLine("   Rotation Length " & GrazingInterval.ToString)
            Console.WriteLine("   Residual " & GrazingResidual.ToString)
            Console.WriteLine("   Stocking Rate " & StockingRate.ToString)
        End If
    End Sub

    <Description("Pasture covers pre-grazing")> _
    <Output()> <Units("kg/ha")> Public ReadOnly Property preGrazeCovers As Double()
        Get
            Return myFarm.preGrazeCovers
        End Get
    End Property

    <Description("Pasture covers post-grazing")> _
    <Output()> <Units("kg/ha")> Public ReadOnly Property postGrazeCovers As Double()
        Get
            Return myFarm.postGrazeCovers
        End Get
    End Property

    <Description("Main model step")> _
    <EventHandler()> Sub OnProcess()
        ' ************* Farm testing **********************
        If Not (GrazingIntervalIsSet) Then
            GrazingInterval = dairyNZ_mg(MyClock.month - 1)
            GrazingIntervalIsSet = False 'not user set so reset switch
        End If
        If Not (GrazingResidualIsSet) Then
            GrazingResidual = dairyNZ_gr(MyClock.month - 1)
            GrazingResidualIsSet = False 'not user set so reset switch
        End If

        myFarm.GrazingResidual = GrazingResidual
        myFarm.GrazingInterval = GrazingInterval
        'myFarm.StockingRate = sr

        If (MyClock.simulation_days = 0) And AllocationType = 1 Then
            'myFarm.CutToFeedWedge(GrazingResidual, GrazingInterval, MilkingCows() / FarmArea, ME_Demand_Cow / 11.5)
            'myFarm.CutToFeedWedge(GrazingResidual, 21)
            myFarm.CutToFeedWedge(GrazingResidual, Rotation)
        End If

        myFarm.Process(is_start_week)

        'Dim cover = myFarm.AverageCover()
        ' ************* Farm testing **********************
        PrepareOutputs()

        'PublishGrazingEvent() - testing
    End Sub

    <Description("Do whole farm fertiliser events apply fertiliser to effluent paddocks")> _
    <Output()> Public ApplyFertToEffluentPdks As Boolean = False
    '<Param()> <Output()> Public ApplyFertToEffluentPdks As Boolean = False
    'Whole farm fertiliser application

    <Description("Whole farm fertiliser event - this will become a manager script")> _
    <EventHandler()> Public Sub OnApplyFertiliser(ByVal amount As FertiliserApplicationType)
        myFertiliserAmount += myFarm.Fertilise(amount, ApplyFertToEffluentPdks)
    End Sub

    <Description("Default [N])")> _
 <Param()> <Output()> Public Property Default_N_Conc__() As Double
        Get
            Return PaddockWrapper.Default_N_Conc
        End Get
        Set(ByVal value As Double)
            PaddockWrapper.Default_N_Conc = value
        End Set
    End Property
    <Description("Default Digestibility)")> _
 <Param()> <Output()> Public Property Default_Digestibility__() As Double
        Get
            Return PaddockWrapper.Default_Digestibility
        End Get
        Set(ByVal value As Double)
            PaddockWrapper.Default_Digestibility = value
        End Set
    End Property
    <Description("Default Fert Application Depth")> _
 <Param()> <Output()> Public Property DefaultApplicationDepth__() As Double
        Get
            Return PaddockWrapper.Default_Application_Depth
        End Get
        Set(ByVal value As Double)
            PaddockWrapper.Default_Application_Depth = value
        End Set
    End Property

    '<Description("Whole farm irrigation application - this will become a manager script")> _
    '<EventHandler()> Public Sub OnApplyIrrigation(ByVal amount As IrrigationApplicationType)
    '    myIrrigationAmount += myFarm.Irrigate(amount, myIrrigation_efficiency)
    'End Sub

    '<Description("Overloaded irrigation Apply event")> _
    '<EventHandler()> Public Sub OnApply(ByVal amount As IrrigationApplicationType)
    '    OnApplyIrrigation(amount)
    'End Sub

    <Description("Overloaded fertiliser Apply event")> _
    <EventHandler()> Public Sub OnApply(ByVal amount As FertiliserApplicationType)
        OnApplyFertiliser(amount)
    End Sub

    '<Description("Amount of water applied in relation to the whole farm")> _
    '<Output()> <Units("mm/ha")> Public ReadOnly Property Irrigation() As Double
    '    Get
    '        Return myIrrigationAmount
    '    End Get
    'End Property

    <Description("Amount of fertiliser applied in relation to the whole farm")> _
    <Output()> <Units("kg/ha")> Public ReadOnly Property Fertiliser() As Double
        Get
            Return myFertiliserAmount
        End Get
    End Property
#End Region

    '<Description("Proportion of applied irrgation that reaches the soil surface")> _
    '<Output()> <Units("0-1")> Public Property irrigation_efficiency() As Double
    '    Get
    '        Return myIrrigation_efficiency
    '    End Get
    '    Set(ByVal value As Double)
    '        If (value < 0) Then
    '            myIrrigation_efficiency = 0
    '        ElseIf (value > 1) Then
    '            myIrrigation_efficiency = 1
    '        Else
    '            myIrrigation_efficiency = value
    '        End If
    '    End Set
    'End Property

    <Description("Effective farm area [ha]")> _
    <Param()> <Output()> <Units("ha")> Public Property FarmArea() As Double
        '<Param()> <Output()> <Units("ha")> Public Property FarmArea() As Double
        '<Param()> <Output()> <Units("ha")> Public Property FarmArea() As Double
        Get
            Return TotalFarmArea
        End Get
        Set(ByVal value As Double)
            If (myFarm IsNot Nothing) Then
                myFarm.FarmArea = value
            End If
            TotalFarmArea = value
        End Set
    End Property

    <Description("Take dry stock off farm")> _
    <Param()> <Output()> <Units("")> Public Property WinterOffDryStock__() As Integer
        '<Description("Take dry stock off farm")> _
        '<Output()> <Units("")> Public Property WinterOffDryStock__() As Integer
        Get
            If (myFarm.boolWinterOffDryStock) Then
                Return 1
            Else
                Return 0
            End If
        End Get
        Set(ByVal value As Integer)
            myFarm.boolWinterOffDryStock = value > 0
        End Set
    End Property

    <Description("Set paddocks avalibility to be included in the grazing rotation")> _
    <Output()> <Units("")> Public Property PaddockGrazable(ByVal i As Integer) As Integer
        Get
            If myFarm.PaddockGrazable(i) Then
                Return 1
            Else
                Return 0
            End If
        End Get
        Set(ByVal value As Integer)
            myFarm.PaddockGrazable(i) = value > 0
        End Set
    End Property

    Sub SetupFarmSim(ByVal MySystem As Simulation)
        Dim FarmSim As Component = CType(MySystem.LinkByType("FarmSimGraze"), Component)
        If FarmSim Is Nothing Then
            Return
        End If

        Dim UI_StockRate As Double
        MySimulation.Get("UI_StockRate", UI_StockRate)
        Dim UI_SuppType As String = Nothing
        MySimulation.Get("UI_SuppType", UI_SuppType)

        'BaseStockingRate = UI_StockRate
        'StockingRate = BaseStockingRate

        StockingRate = UI_StockRate

        Dim GrassSilage As Integer = 0
        Dim GrainOrConcentrate As Integer = 1
        If (UI_SuppType = "GrassSilage") Then
            myFarm.SupplementME = 10
            myFarm.SupplementN = 0.035
            myFarm.SupplementWastage = 0.0
            myFarm.SupplementDigestibility = 0.7 ' minimum for high quality silage. Source: DairyNZ FarmFact 1-46
        Else 'GrainOrConcentrate
            myFarm.SupplementME = 12
            myFarm.SupplementN = 0.018
            myFarm.SupplementWastage = 0.0
            myFarm.SupplementDigestibility = 0.8
        End If

        SilageStoreEnable = 0 'False 'all supplement purchase / no silage kept
        WinterOffDryStock__ = 1 'all stock wintered off farm
        default_gr = Val_gr
    End Sub


#Region "CowProperties"
    <Description("Stocking is the actual number of cows on farm i.e. normal stocking rate less cows wintering off")> _
    <Output()> <Units("cows/ha")> Public Property StockingRate() As Double
        Get
            Return myFarm.StockingRate
        End Get
        Set(ByVal value As Double)
            If (value >= 0) Then
                myFarm.StockingRate = value
            End If
        End Set
    End Property

    'Private BaseStockingRate As Double = 2.5 'default value
    ''Stocking rate a peak of lactation
    '<Output()> <Units("cows/ha")> Public Property PeakStockingRate() As Single
    '        Get
    '                Return BaseStockingRate
    '        End Get
    '        Set(ByVal value As Single)
    '                If (value >= 0) Then
    '                        BaseStockingRate = value
    '                Else
    '                        BaseStockingRate = 0
    '                End If
    '        End Set
    'End Property

    <Description("Stocking rate a peak of lactation")> _
    <Output()> <Units("cows/ha")> Public Property PeakStockingRate() As Double
        Get
            Return CSng(StockingRate)
        End Get
        Set(ByVal value As Double)
            If (value >= 0) Then
                StockingRate = value
            Else
                StockingRate = 0
            End If
        End Set
    End Property

    <Description("ME still required to meet animal requirements")> _
    <Output()> <Units("MJME")> Public ReadOnly Property RemainingFeedDemand() As Double
        Get
            Return myHerd.RemainingFeedDemand
        End Get
    End Property

    <Description("Total animal requirements [MJME] (for the full herd)")> _
    <Output()> <Units("Cows")> Public ReadOnly Property TotalCows() As Double
        Get
            Return myFarm.TotalCows
        End Get
    End Property

    <Description("Total animal requirements [MJME] (for the full herd)")> _
    <Output()> <Units("MJME")> Public ReadOnly Property TotalFeedDemand() As Double
        Get
            Return myHerd.TodaysEnergyRequirement
        End Get
    End Property

#Region "ME Requirements (Herd)"
    <Description("Total Metabolisable energy required by herd [MJME/day]")> _
    <Output()> <Units("MJME")> Public ReadOnly Property ME_Demand_Total() As Double
        Get
            Return myHerd.ME_Total
        End Get
    End Property

    <Description("Metabolisable energy required by herd for maintance [MJME/day]")> _
    <Output()> <Units("MJME")> Public ReadOnly Property ME_Demand_Maintance() As Double
        Get
            Return myHerd.ME_Maintance
        End Get
    End Property

    <Description("Metabolisable energy required by herd for lactation [MJME/day]")> _
    <Output()> <Units("MJME")> Public ReadOnly Property ME_Demand_Lactation() As Double
        Get
            Return myHerd.ME_Lactation
        End Get
    End Property
    <Description("Metabolisable energy required by herd for pregnancy [MJME/day]")> _
    <Output()> <Units("MJME")> Public ReadOnly Property ME_Demand_Pregnancy() As Double
        Get
            Return myHerd.ME_Pregnancy
        End Get
    End Property
    <Description("Metabolisable energy required by herd for walking [MJME/day]")> _
    <Output()> <Units("MJME")> Public ReadOnly Property ME_Demand_Walking() As Double
        Get
            Return myHerd.ME_Walking
        End Get
    End Property
    <Description("Metabolisable energy required by herd for live weight change [MJME/day]")> _
    <Output()> <Units("MJME")> Public ReadOnly Property ME_Demand_WeightChange() As Double
        Get
            Return myHerd.ME_WeightChange
        End Get
    End Property
#End Region
#Region "ME Requirements (Cow)"
    <Description("Total Metabolisable energy required [MJME/cow/day]")> _
    <Output()> <Units("MJME/cow")> Public ReadOnly Property ME_Demand_Total_Cow() As Double
        Get
            Return myHerd.ME_Total_Cow
        End Get
    End Property

    <Description("Metabolisable energy required for maintainence [MJME/cow/day]")> _
    <Output()> <Units("MJME/cow")> Public ReadOnly Property ME_Demand_Maintance_Cow() As Double
        Get
            Return myHerd.ME_Maintance_Cow
        End Get
    End Property
    <Description("Metabolisable energy required for lactation [MJME/cow/day]")> _
    <Output()> <Units("MJME/cow")> Public ReadOnly Property ME_Demand_Lactation_Cow() As Double
        Get
            Return myHerd.ME_Lactation_Cow
        End Get
    End Property
    <Description("Metabolisable energy required for live weight change [MJME/cow/day]")> _
    <Output()> <Units("MJME/cow")> Public ReadOnly Property ME_Demand_WeightChange_Cow() As Double
        Get
            Return myHerd.ME_WeightChange_Cow
        End Get
    End Property
    <Description("Metabolisable energy required for pregnancy [MJME/cow/day]")> _
    <Output()> <Units("MJME/cow")> Public ReadOnly Property ME_Demand_Pregnancy_Cow() As Double
        Get
            Return myHerd.ME_Pregnancy_Cow
        End Get
    End Property
    <Description("Metabolisable energy required for walking [MJME/cow/day]")> _
    <Output()> <Units("MJME/cow")> Public ReadOnly Property ME_Demand_Walking_Cow() As Double
        Get
            Return myHerd.ME_Walking_Cow
        End Get
    End Property
#End Region
    <Description("Milk solids production - total")> _
    <Output()> <Units("kgMS/day")> Public ReadOnly Property MilkSolids() As Double
        Get
            Return myHerd.MS_per_Day
        End Get
    End Property

    <Description("Milk solids production - total")> _
    <Output()> <Units("kgMS/day")> Public ReadOnly Property MilkSolids_Ha() As Double
        Get
            Return myHerd.MS_per_Day_Cow * StockingRate
        End Get
    End Property

    <Description("Milk solids production per cow")> _
    <Output()> <Units("kgMS/cow/day")> Public ReadOnly Property MilkSolids_Cow() As Double
        Get
            Return myHerd.MS_per_Day_Cow
        End Get
    End Property

    ''Month of lactation
    '<Output()> <Units("")> Public ReadOnly Property MLact() As Double
    '        Get
    '                Return myHerd.Month_Of_Lactation
    '        End Get
    'End Property

    ''Month of pregnancy
    '<Output()> <Units("")> Public ReadOnly Property MPreg() As Double
    '        Get
    '                Return myHerd.Month_Of_Pregnancy
    '        End Get
    'End Property

    <Description("Average cow live weight")> _
    <Output()> <Units("kg_LWt/Cow")> Public ReadOnly Property Cow_LWt() As Double
        Get
            Return myHerd.Live_Weight
        End Get
    End Property

    <Description("Average cows change in live weight")> _
    <Output()> <Units("kg_LWt/cow/day")> Public ReadOnly Property Cow_dLWt() As Double
        Get
            Return myHerd.LWt_Change
        End Get
    End Property
    <Description("Averge body condition score")> _
     <Output()> <Units("")> Public ReadOnly Property Cow_BC() As Double
        Get
            Return myHerd.BC  'Reference cow ConditionScore
        End Get
    End Property
    <Description("Averge body condition score")> _
    <Param()> <Output()> <Units("")> Public Property Cow_BC_ByMonth() As String
        '<Description("Averge body condition score")> _
        '<Output()> <Units("")> Public Property Cow_BC_ByMonth() As String
        Get
            Return myHerd.Cow_BC_ByMonth_
        End Get
        Set(ByVal value As String)
            myFarm.setCow_BC(StringtoDoubleArray(value))
        End Set
    End Property

    <Description("Is herd currently dried off")> _
    <Output()> <Units("")> Public ReadOnly Property Cow_IsDry() As Integer
        Get
            If (myHerd.isDry) Then
                Return 1
            Else
                Return 0
            End If
        End Get
    End Property
#End Region

    <Description("Current grazing interval / return period")> _
    <Output()> <Units("Days")> Public Property GrazingInterval() As Integer
        Get
            Return myFarm.GrazingInterval
        End Get
        Set(ByVal value As Integer)
            GrazingIntervalIsSet = True
            myFarm.GrazingInterval = value
        End Set
    End Property

    <Description("Current cow grazing residual")> _
    <Output()> <Units("kgDM/ha")> Public Property GrazingResidual() As Integer
        Get
            Return myFarm.GrazingResidual
        End Get
        Set(ByVal value As Integer)
            GrazingResidualIsSet = True
            myFarm.GrazingResidual = value
        End Set
    End Property

    <Description("Quantity of silage cut on farm today [kgDM/day]")> _
    <Output()> <Units("kgDM")> Public ReadOnly Property SilageCut_kg() As Double
        Get
            Return myFarm.SilageCut
        End Get
    End Property

    <Description("Quantity of silage cut on farm today [kgDM/ha/day]")> _
    <Output()> <Units("kgDM/ha")> Public ReadOnly Property SilageCut_kgha() As Double
        Get
            Return SilageCut_kg / FarmArea
        End Get
    End Property

#Region "2: Feeding Supplements"

    <Description("Quantity of silage fed to cows today [kgDM/ha/day]")> _
    <Output()> <Units("kgDM/ha")> Public ReadOnly Property SupplementFedOut_kgha() As Double
        Get
            Return SupplementFedOut / FarmArea
        End Get
    End Property

    <Description("Quantity of silage fed to cows today [kgDM/ha/day]")> _
    <Output()> <Units("kgDM/ha")> Public ReadOnly Property SilageFedOut_kgha() As Double
        Get
            Return SilageFedOut / FarmArea
        End Get
    End Property

    <Description("Quantity of silage fed to cows today [kgDM/day]")> _
    <Output()> <Units("kgDM")> Public ReadOnly Property SupplementFedOut() As Double
        Get
            Return myFarm.SupplementFedOut
        End Get
    End Property

    <Description("Quantity of silage fed to cows today [kgDM/day]")> _
    <Output()> <Units("kgDM")> Public ReadOnly Property SilageFedOut() As Double
        Get
            Return myFarm.SilageFedOut
        End Get
    End Property

    <Description("Energy content of purchased supplement")> _
    <Param()> <Output()> <Units("MJME/kgDM")> Public Property SupplementME__() As Double
        '<Description("Energy content of purchased supplement")> _
        '<Output()> <Units("MJME/kgDM")> Public Property SupplementME__() As Double
        Get
            Return myFarm.SupplementME
        End Get
        Set(ByVal value As Double)
            myFarm.SupplementME = value
        End Set
    End Property

    <Description("Nitrogen content of purchased supplement")> _
    <Param()> <Output()> <Units("kgN/kgDM")> Public Property SupplementN__() As Double
        '<Description("Nitrogen content of purchased supplement")> _
        '<Output()> <Units("kgN/kgDM")> Public Property SupplementN__() As Double
        Get
            Return myFarm.SupplementN
        End Get
        Set(ByVal value As Double)
            myFarm.SupplementN = value
        End Set
    End Property

    <Description("Supplement Digestibility")> _
    <Param()> <Output()> <Units("0-1")> Public Property SupplementDigestibility__() As Double
        '<Description("Supplement Digestibility")> _
        '<Output()> <Units("0-1")> Public Property SupplementDigestibility__() As Double
        Get
            Return myFarm.SupplementDigestibility
        End Get
        Set(ByVal value As Double)
            myFarm.SupplementDigestibility = value
        End Set
    End Property

    <Description("Supplment loss at feeding out")> _
    <Param()> <Output()> <Units("%")> Public Property SupplementWastage__() As Double
        '<Description("Supplment loss at feeding out")> _
        '<Output()> <Units("%")> Public Property SupplementWastage__() As Double
        Get
            Return myFarm.SupplementWastage * 100
        End Get
        Set(ByVal value As Double)
            myFarm.SupplementWastage = value / 100
        End Set
    End Property

    <Description("Energy content of purchased supplement")> _
    <Param()> <Output()> <Units("MJME/kgDM")> Public Property GrassSilageSupplementME__() As Double
        '<Description("Energy content of purchased supplement")> _
        '<Output()> <Units("MJME/kgDM")> Public Property GrassSilageSupplementME__() As Double
        Get
            Return myFarm.GrassSilageSupplementME
        End Get
        Set(ByVal value As Double)
            myFarm.GrassSilageSupplementME = value
        End Set
    End Property

    <Description("Nitrogen content of purchased supplement")> _
    <Param()> <Output()> <Units("kgN/kgDM")> Public Property GrassSilageSupplementN__() As Double
        '<Description("Nitrogen content of purchased supplement")> _
        '<Output()> <Units("kgN/kgDM")> Public Property GrassSilageSupplementN__() As Double

        Get
            Return myFarm.GrassSilageSupplementN
        End Get
        Set(ByVal value As Double)
            myFarm.GrassSilageSupplementN = value
        End Set
    End Property

    <Description("Supplement Digestibility")> _
    <Param()> <Output()> <Units("0-1")> Public Property GrassSilageSupplementDigestibility__() As Double
        '<Description("Supplement Digestibility")> _
        '<Output()> <Units("0-1")> Public Property GrassSilageSupplementDigestibility__() As Double
        Get
            Return myFarm.GrassSilageSupplementDigestibility
        End Get
        Set(ByVal value As Double)
            myFarm.GrassSilageSupplementDigestibility = value
        End Set
    End Property

    <Description("Supplement loss at feeding out")> _
    <Param()> <Output()> <Units("%")> Public Property GrassSilageSupplementWastage__() As Double
        '<Description("Supplement loss at feeding out")> _
        '<Output()> <Units("%")> Public Property GrassSilageSupplementWastage__() As Double
        Get
            Return myFarm.GrassSilageSupplementWastage * 100
        End Get
        Set(ByVal value As Double)
            myFarm.GrassSilageSupplementWastage = value / 100
        End Set
    End Property

    <Description("Energy content of purchased supplement")> _
    <Param()> <Output()> <Units("MJME/kgDM")> Public Property GrainOrConcentrateSupplementME__() As Double
        '<Description("Energy content of purchased supplement")> _
        '<Output()> <Units("MJME/kgDM")> Public Property GrainOrConcentrateSupplementME__() As Double
        Get
            Return myFarm.GrainOrConcentrateSupplementME
        End Get
        Set(ByVal value As Double)
            myFarm.GrainOrConcentrateSupplementME = value
        End Set
    End Property

    <Description("Nitrogen content of purchased supplement")> _
    <Param()> <Output()> <Units("kgN/kgDM")> Public Property GrainOrConcentrateSupplementN__() As Double
        '<Description("Nitrogen content of purchased supplement")> _
        '<Output()> <Units("kgN/kgDM")> Public Property GrainOrConcentrateSupplementN__() As Double
        Get
            Return myFarm.GrainOrConcentrateSupplementN
        End Get
        Set(ByVal value As Double)
            myFarm.GrainOrConcentrateSupplementN = value
        End Set
    End Property

    <Description("Supplement Digestibility")> _
    <Param()> <Output()> <Units("0-1")> Public Property GrainOrConcentrateSupplementDigestibility__() As Double
        '<Description("Supplement Digestibility")> _
        '<Output()> <Units("0-1")> Public Property GrainOrConcentrateSupplementDigestibility__() As Double
        Get
            Return myFarm.GrainOrConcentrateSupplementDigestibility
        End Get
        Set(ByVal value As Double)
            myFarm.GrainOrConcentrateSupplementDigestibility = value
        End Set
    End Property

    <Description("Supplement loss at feeding out")> _
    <Param()> <Output()> <Units("%")> Public Property GrainOrConcentrateSupplementWastage__() As Double
        '<Description("Supplement loss at feeding out")> _
        '<Output()> <Units("%")> Public Property GrainOrConcentrateSupplementWastage__() As Double

        Get
            Return myFarm.GrainOrConcentrateSupplementWastage * 100
        End Get
        Set(ByVal value As Double)
            myFarm.GrainOrConcentrateSupplementWastage = value / 100
        End Set
    End Property

    <Description("Proportion of silage lost during feeding out (Default = 15%, source DairyNZ)")> _
    <Param()> <Output()> <Units("0-1")> Public Property SilageWastage__() As Double
        '<Description("Proportion of silage lost during feeding out (Default = 15%, source DairyNZ)")> _
        '<Output()> <Units("0-1")> Public Property SilageWastage__() As Double
        Get
            Return myFarm.SilageWastage * 100
        End Get
        Set(ByVal value As Double)
            myFarm.SilageWastage = value / 100
        End Set
    End Property

    <Description("Silage Digestibility (could this be made read only once the store has been refactored)")> _
    <Param()> <Output()> <Units("0-1")> Public Property SilageDigestibility__() As Double
        '<Description("Silage Digestibility (could this be made read only once the store has been refactored)")> _
        '<Output()> <Units("0-1")> Public Property SilageDigestibility__() As Double
        Get
            Return myFarm.SilageDigestibility
        End Get
        Set(ByVal value As Double)
            myFarm.SilageDigestibility = value
        End Set
    End Property

    <Description("proportion of pasture ME captured during silage making [default = 0.9, e.g. 12 ME pasture produces 10.8 ME silage")> _
    <Param()> <Output()> <Units("0-1")> Public Property SilageQualityModifier__() As Double
        '<Description("proportion of pasture ME captured during silage making [default = 0.9, e.g. 12 ME pasture produces 10.8 ME silage")> _
        '<Output()> <Units("0-1")> Public Property SilageQualityModifier__() As Double
        Get
            Return myFarm.SilageQualityModifier
        End Get
        Set(ByVal value As Double)
            myFarm.SilageQualityModifier = Math.Max(0, Math.Min(1, value))
        End Set
    End Property

    <Description("Energy content of silage produced on farm")> _
    <Param()> <Output()> <Units("MJME/kgDM")> Public Property SilageME__() As Double
        '<Description("Energy content of silage produced on farm")> _
        '<Output()> <Units("MJME/kgDM")> Public Property SilageME__() As Double
        Get
            Return myFarm.SilageME
        End Get
        Set(ByVal value As Double)
            myFarm.SilageME = value
        End Set
    End Property

    <Description("Nitrogen content of silage produced on farm")> _
    <Param()> <Output()> <Units("kgN/kgDM")> Public Property SilageN__() As Double
        '<Description("Nitrogen content of silage produced on farm")> _
        '<Output()> <Units("kgN/kgDM")> Public Property SilageN__() As Double
        Get
            Return myFarm.SilageN
        End Get
        Set(ByVal value As Double)
            myFarm.SilageN = value
        End Set
    End Property

    <Description("Supplment/silage loss during cutting")> _
    <Param()> <Output()> <Units("%")> Public Property SilageCutWastage__() As Double
        '<Description("Supplment/silage loss during cutting")> _
        '<Output()> <Units("%")> Public Property SilageCutWastage__() As Double
        Get
            Return myFarm.SilageCutWastage * 100
        End Get
        Set(ByVal value As Double)
            myFarm.SilageCutWastage = value / 100
        End Set
    End Property

#End Region

#Region "3: Conservation"
    Private mySilageStoreEnable As Integer = 0
    Private mySilageStore As Integer = 0
    ' Conservation trigger pasture mass (Dawn default = 3500)
    '<Output()> <Units("kgDM/ha")> Public CDM As Double = 3500
    ' Conservation cutting residual pasture mass (Dawn default = 1600)
    '<Output()> <Units("kgDM/ha")> Public CR As Integer = 1600
    'First Conservation Date - uing a month for the time being
    '<Output()> <Units("")> Public FCD As Integer = 9
    'Last Conservation Date - uing a month for the time being
    '<Output()> <Units("")> Public LCD As Integer = 3


    <Description("First Conservation Date")> _
    <Output()> <Units("")> Public Property ConservationStart() As String
        Get
            Return myFarm.FCD.ToString("dd-MMM")
        End Get
        Set(ByVal value As String)
            myFarm.FCD = Date.Parse(value)
        End Set
    End Property

    <Description("Last Conservation Date")> _
    <Output()> <Units("")> Public Property ConservationFinish() As String
        Get
            Return myFarm.LCD.ToString("dd-MMM")
        End Get
        Set(ByVal value As String)
            myFarm.LCD = Date.Parse(value)
        End Set
    End Property

    <Description("Storage of cut silage on farm (for later use)")> _
    <Output()> <Units("")> Public Property SilageStoreEnable() As Integer
        Get
            If myFarm.EnableSilageStore Then
                Return 1
            Else
                Return 0

            End If
        End Get
        Set(ByVal value As Integer)
            myFarm.EnableSilageStore = value > 0
        End Set
    End Property

    <Description("Amount of silage avalible for feeding out [kgDM]")> _
    <Output()> <Units("kgDM")> Public Property SilageStore() As Double
        Get
            Return myFarm.SilageStore
        End Get
        Set(ByVal value As Double)
            myFarm.SilageStore = value
        End Set
    End Property

    <Description("Amount of silage avalible for feeding out [kgDM]")> _
    <Output()> <Units("kgDM/ha")> Public Property SilageStore_kgha() As Double
        Get
            Return SilageStore() / FarmArea()
        End Get
        Set(ByVal value As Double)
            SilageStore = value * FarmArea()
        End Set
    End Property

    <Description("Conservation trigger mass (close paddocks above this residual)")> _
    <Output()> <Units("kgDM/ha")> Public Property ConservationTrigger() As Double
        Get
            Return myFarm.CDM
        End Get
        Set(ByVal value As Double)
            myFarm.CDM = value
        End Set
    End Property

    <Description("Conservation cutting residual")> _
    <Output()> <Units("kgDM/ha")> Public Property ConservationResidual() As Double
        Get
            Return myFarm.CR
        End Get
        Set(ByVal value As Double)
            myFarm.CR = CInt(value)
        End Set
    End Property
#End Region

#Region "Additional Output Variables"
    Private Sub PrepareOutputs()
        myFarm.PrepareOutputs()

        DM_Eaten = CSng(myFarm.DM_Eaten)
        DM_Eaten_Pasture = CSng(myFarm.DM_Eaten_Pasture)
        DM_Eaten_Silage = CSng(myFarm.DM_Eaten_Silage)
        DM_Eaten_Supplement = CSng(myFarm.DM_Eaten_Supplement)
        ME_Demand = CSng(myFarm.ME_Demand)
        ME_Eaten = CSng(myFarm.ME_Eaten)
        ME_Eaten_Pasture = CSng(myFarm.ME_Eaten_Pasture)
        ME_Eaten_Silage = CSng(myFarm.ME_Eaten_Silage)
        ME_Eaten_Supplement = CSng(myFarm.ME_Eaten_Supplement)
        N_Eaten = CSng(myFarm.N_Eaten)
        N_Eaten_Pasture = CSng(myFarm.N_Eaten_Pasture)
        N_Eaten_Silage = CSng(myFarm.N_Eaten_Silage)
        N_Eaten_Supplement = CSng(myFarm.N_Eaten_Supplement)
        N_to_milk = CSng(myFarm.N_to_milk)
        N_to_BC = CSng(myFarm.N_to_BC)
        N_to_faeces = CSng(myFarm.N_to_faeces)
        DM_to_faeces = CSng(myFarm.DM_to_faeces)
        N_to_urine = CSng(myFarm.N_to_urine)
        N_Balance = CSng(myFarm.N_Balance)
        N_Out = CSng(myFarm.N_Out)

        ME_Demand_Cow = CSng(myHerd.ME_Demand_Cow())
        ME_Eaten_Cow = CSng(myHerd.ME_Eaten_Cow())
        ME_Eaten_Pasture_Cow = CSng(myHerd.ME_Eaten_Pasture_Cow())
        ME_Eaten_Supplement_Cow = CSng(myHerd.ME_Eaten_Supplement_Cow())
        DM_Eaten_Cow = CSng(myHerd.DM_Eaten_Cow())
        DM_Eaten_Pasture_Cow = CSng(myHerd.DM_Eaten_Pasture_Cow())
        DM_Eaten_Supplement_Cow = CSng(myHerd.DM_Eaten_Supplement_Cow())
        N_Eaten_Cow = CSng(myHerd.N_Eaten_Cow())
        N_Eaten_Pasture_Cow = CSng(myHerd.N_Eaten_Pasture_Cow())
        N_Eaten_Supplement_Cow = CSng(myHerd.N_Eaten_Supplement_Cow())
        N_to_milk_Cow = CSng(myHerd.N_to_milk_Cow())
        N_to_BC_Cow = CSng(myHerd.N_to_BC_Cow())
        N_to_faeces_Cow = CSng(myHerd.N_to_faeces_Cow())
        N_to_urine_Cow = CSng(myHerd.N_to_urine_Cow())
    End Sub

    '<Output()> <Units("MJME/ha")> Public ME_Demand As Single
    '<Output()> <Units("MJME/ha")> Public ME_Eaten As Single
    '<Output()> <Units("MJME/ha")> Public ME_Eaten_Pasture As Single
    '<Output()> <Units("kgDM/ha")> Public ME_Eaten_Supplement As Single
    '<Output()> <Units("kgDM/ha")> Public DM_Eaten As Single
    '<Output()> <Units("kgDM/ha")> Public DM_Eaten_Pasture As Single
    '<Output()> <Units("kgDM/ha")> Public DM_Eaten_Supplement As Single
    '<Output()> <Units("kgN/ha")> Public N_Eaten As Single
    '<Output()> <Units("kgN/ha")> Public N_Eaten_Pasture As Single
    '<Output()> <Units("kgN/ha")> Public N_Eaten_Supplement As Single
    '<Output()> <Units("kgN/ha")> Public N_to_milk As Single
    '<Output()> <Units("kgN/ha")> Public N_to_BC As Single
    '<Output()> <Units("kgN/ha")> Public N_to_feaces As Single
    '<Output()> <Units("kgN/ha")> Public N_to_urine As Single

    <Description("Total nitrogen balance of herd")> _
    <Output()> <Units("kgN/ha")> Public N_Balance As Single
    <Description("Nitrogen output of herd")> _
    <Output()> <Units("kgN/ha")> Public N_Out As Single
    <Description("Energy demand")> _
    <Output()> <Units("MJME/ha")> Public ME_Demand As Single
    <Description("Total Energy consumed")> _
    <Output()> <Units("MJME/ha")> Public ME_Eaten As Single
    <Description("Energy consumed as silage")> _
    <Output()> <Units("MJME/ha")> Public ME_Eaten_Pasture As Single
    <Description("Energy consumed as pasture")> _
    <Output()> <Units("MJME/ha")> Public ME_Eaten_Silage As Single
    <Description("Energy consumed as supplement")> _
    <Output()> <Units("MJME/ha")> Public ME_Eaten_Supplement As Single
    <Description("Total dry matter consumed")> _
    <Output()> <Units("kgDM/ha")> Public DM_Eaten As Single
    <Description("Dry matter consumed as pasture")> _
    <Output()> <Units("kgDM/ha")> Public DM_Eaten_Pasture As Single
    <Description("Dry matter consumed as silage")> _
    <Output()> <Units("kgDM/ha")> Public DM_Eaten_Silage As Single
    <Description("Dry matter consumed as supplement")> _
    <Output()> <Units("kgDM/ha")> Public DM_Eaten_Supplement As Single
    <Description("Total nitrogen consumed")> _
    <Output()> <Units("kgN/ha")> Public N_Eaten As Single
    <Description("Nitrogen consumed form pasture")> _
    <Output()> <Units("kgN/ha")> Public N_Eaten_Pasture As Single
    <Description("Nitrogen consumed form silage")> _
    <Output()> <Units("kgN/ha")> Public N_Eaten_Silage As Single
    <Description("Nitrogen consumed form supplement")> _
    <Output()> <Units("kgN/ha")> Public N_Eaten_Supplement As Single
    <Description("Nitrogen partitioned to milk")> _
    <Output()> <Units("kgN/ha")> Public N_to_milk As Single
    <Description("Nitrogen partitioned to body condition")> _
    <Output()> <Units("kgN/ha")> Public N_to_BC As Single
    <Description("Nitrogen partitioned to dung")> _
    <Output()> <Units("kgN/ha")> Public N_to_faeces As Single
    <Description("Nitrogen partitioned to urine")> _
    <Output()> <Units("kgN/ha")> Public N_to_urine As Double
    <Description("Drymatter partitioned to dung")> _
    <Output()> <Units("kgDM/ha")> Public DM_to_faeces As Single

    <Description("Current status of paddocks")> _
    <Output()> <Units("")> Public ReadOnly Property PaddockStatus() As String()
        Get
            Return myFarm.PaddockStatus
        End Get
    End Property

    <Description("Energy consumed as pasture per paddock")> _
    <Output()> <Units("kgDM/ha/day")> Public ReadOnly Property AverageGrowthRate() As Single
        Get
            Return CSng(myFarm.AverageGrowthRate)
        End Get
    End Property

    <Description("Energy consumed as pasture per paddock")> _
    <Output()> <Units("MJME/ha")> Public ReadOnly Property ME_Eaten_Pasture_Pdks() As Single()
        Get
            Return myFarm.ME_Eaten_Pasture_Pdks
        End Get
    End Property
    <Description("Energy consumed as Supplements per paddock")> _
    <Output()> <Units("MJME/ha")> Public ME_Eaten_Supplement_Pdks As Single()

    <Description("Energy consumed per paddock")> _
    <Output()> <Units("MJME/ha")> Public ReadOnly Property ME_Eaten_Pdks() As Single()
        Get
            Return myFarm.ME_Eaten_Pdks
        End Get
    End Property
    <Description("Total Dry matter consumed per paddock [kgDM/day]")> _
    <Output()> <Units("kgDM")> Public ReadOnly Property DM_Eaten_Pdks() As Single()
        Get
            Return myFarm.DM_Eaten_Pdks
        End Get
    End Property

    <Description("Dry matter consumed per paddock [kgDM/ha/day]")> _
    <Output()> <Units("kgDM/ha")> Public ReadOnly Property DM_Eaten_Pdks_ha() As Single()
        Get
            Return myFarm.DM_Eaten_Pdks_Ha
        End Get
    End Property

    <Description("Dry matter consumed as pasture per paddock")> _
    <Output()> <Units("kgDM/ha")> Public DM_Eaten_Pasture_Pdks As Single()
    <Description("Dry matter consumed as supplement per paddock")> _
    <Output()> <Units("kgDM/ha")> Public DM_Eaten_Supplement_Pdks As Single()
    <Description("Nitrogen consumed per paddock")> _
    <Output()> <Units("kgN/ha")> Public ReadOnly Property N_Eaten_Pdks() As Single()
        Get
            Return myFarm.N_Eaten_Pdks
        End Get
    End Property
    <Description("Nitrogen consumed as pasture per paddock")> _
    <Output()> <Units("kgN/ha")> Public N_Eaten_Pasture_Pdks As Single()
    <Description("Nitrogen consumed as supplement per paddock")> _
    <Output()> <Units("kgN/ha")> Public N_Eaten_Supplement_Pdks As Single()
    <Description("Nitrogen partitioned to milk per paddock")> _
    <Output()> <Units("kgN/ha")> Public N_to_milk_Pdks As Single()
    <Description("Nitrogen partitioned to body contition per paddock")> _
    <Output()> <Units("kgN/ha")> Public N_to_BC_Pdks As Single()
    <Description("Nitrogen partitioned to dung per paddock")> _
    <Output()> <Units("kgN/ha")> Public ReadOnly Property N_to_faeces_Pdks() As Single()
        Get
            Return myFarm.N_to_faeces_Pdks
        End Get
    End Property
    <Description("Nitrogen partitioned to urine per paddock")> _
    <Output()> <Units("kgN/ha")> Public ReadOnly Property N_to_urine_Pdks() As Double()
        Get
            Return myFarm.N_to_urine_Pdks
        End Get
    End Property
    <Description("Energy demand per cow")> _
    <Output()> <Units("MJME/cow")> Public ME_Demand_Cow As Single
    <Description("Energy consumed per cow")> _
    <Output()> <Units("MJME/cow")> Public ME_Eaten_Cow As Single
    <Description("Energy consumed as pasture per cow")> _
    <Output()> <Units("MJME/cow")> Public ME_Eaten_Pasture_Cow As Single
    <Description("Energy consumed as supplement per cow")> _
    <Output()> <Units("MJME/cow")> Public ME_Eaten_Supplement_Cow As Single
    <Description("Dry matter consumed per cow")> _
    <Output()> <Units("kgDM/cow")> Public DM_Eaten_Cow As Single
    <Description("Dry matter consumed as pasture per cow")> _
    <Output()> <Units("kgDM/cow")> Public DM_Eaten_Pasture_Cow As Single
    <Description("Dry matter consumed as supplement per cow")> _
    <Output()> <Units("kgDM/cow")> Public DM_Eaten_Supplement_Cow As Single
    <Description("Nitrogen consumed per cow")> _
    <Output()> <Units("kgN/cow")> Public N_Eaten_Cow As Single
    <Description("Nitrogen consumed as pasture per cow")> _
    <Output()> <Units("kgN/cow")> Public N_Eaten_Pasture_Cow As Single
    <Description("Nitrogen consumed as supplement per cow")> _
    <Output()> <Units("kgN/cow")> Public N_Eaten_Supplement_Cow As Single
    <Description("Nitrogen partitioned to milk per cow")> _
    <Output()> <Units("kgN/cow")> Public N_to_milk_Cow As Single
    <Description("Nitrogen partitioned to body condition per cow")> _
    <Output()> <Units("kgN/cow")> Public N_to_BC_Cow As Single
    <Description("Nitrogen partitioned to dung per cow")> _
    <Output()> <Units("kgN/cow")> Public N_to_faeces_Cow As Single
    <Description("Nitrogen partitioned to urine per cow")> _
    <Output()> <Units("kgN/cow")> Public N_to_urine_Cow As Double
    'Daily per cow live weight change
#End Region

#Region "4. Management Options"
#Region "4a. Wintering Off Farm"
    ' two options
    ' 1) winter off dry stock
    ' 2) winter off between specific dates
    <Description("Proportion (PWO) of herd wintered off-farm (range 0-1.0)")> _
    <Param()> <Output()> <Units("0-1")> Public Property PWO__() As Single
        '<Description("Proportion (PWO) of herd wintered off-farm (range 0-1.0)")> _
        '<Output()> <Units("0-1")> Public Property PWO__() As Single
        Get
            Return myFarm.PWO
        End Get
        Set(ByVal value As Single)
            myFarm.PWO = value
        End Set
    End Property
    <Description("Date commence winter off")> _
    <Param()> <Output()> <Units("")> Public Property DCWO__() As String
        '<Description("Date commence winter off")> _
        '<Output()> <Units("")> Public Property DCWO__() As String
        Get
            Return myFarm.DCWO.ToString()
        End Get
        Set(ByVal value As String)
            myFarm.DCWO = Date.Parse(value)
        End Set
    End Property
    <Description("Date to stop winter off")> _
    <Param()> <Output()> <Units("")> Public Property DSWO__() As String
        '<Description("Date to stop winter off")> _
        '<Output()> <Units("")> Public Property DSWO__() As String
        Get
            Return myFarm.DSWO.ToString()
        End Get
        Set(ByVal value As String)
            myFarm.DSWO = Date.Parse(value)
        End Set
    End Property

    <Description("Percentage wintered off")> _
    <Output()> <Units("%")> Public ReadOnly Property PercentageWinteredOff() As Double
        Get
            Return (myFarm.DryCows() / myFarm.TotalCows) * 100
        End Get
    End Property

    <Description("Proportion of farm allocated to laneways (not grazed)")> _
    <Output()> <Units("0-1")> Public Property ProportionOfFarmInLaneWays() As Double
        Get
            Return myFarm.ProportionOfFarmInLaneWays()
        End Get
        Set(ByVal value As Double)
            myFarm.ProportionOfFarmInLaneWays = value
        End Set
    End Property

    <Description("Hours per day cows are on laneways")> _
    <Output()> <Units("0-24")> Public Property HoursOnLaneWays() As Double
        Get
            Return myFarm.HoursOnLaneWays()
        End Get
        Set(ByVal value As Double)
            myFarm.HoursOnLaneWays = value
        End Set
    End Property

    <Description("Hours per day cows are in the dairy shed")> _
    <Output()> <Units("0-24")> Public Property HoursInDairyShed() As Double
        Get
            Return myFarm.HoursInDairyShed()
        End Get
        Set(ByVal value As Double)
            myFarm.HoursInDairyShed = value
        End Set
    End Property

    <Description("Amount of effluient being held in storage")> _
    <Output()> <Units("TBC")> Public ReadOnly Property EffluientPondVolume() As Double
        Get
            Return myEffluentPond.Volume
        End Get
    End Property
#End Region

#Region "4b. USe stand-off (assumes that cows still eat pasture)"
#End Region
#End Region

    Private Sub PrintFarmSummary()
        Console.WriteLine()
        Console.WriteLine("---------- DDRules Initialisation ----------")
        myFarm.PrintFarmSummary()
        Console.WriteLine("     Debug Switches")
        Console.WriteLine("             Debug Level   " & DebugLevel)
        Console.WriteLine("             Break Feeding " & BreakFeeding)
        Console.WriteLine("             Avg length    " & GrowthRateWindowSize)
    End Sub

    <Description("Detail level of debug output (0=none, 1=some, 2=detailed, 3=very detailed)")> _
    <Param()> <Output()> Property DebugLevel() As Integer
        Get
            Return myDebugLevel
        End Get
        Set(ByVal value As Integer)
            myDebugLevel = value
            myFarm.DebugLevel = value - 1
        End Set
    End Property

    <Description("Interpolate monthly values to get daily values (warning: this can cause issues) [0=No, 1=Yes]")> _
    <Param()> Property Cow_Interpolation() As Integer
        Get
            Return Convert.ToInt32(SimpleCow.DoInterpolate)
        End Get
        Set(ByVal value As Integer)
            SimpleCow.DoInterpolate = (value = 1)
        End Set
    End Property
    Private Function MyCStr(ByVal d As Integer) As String
        Return CStr(d)
    End Function
    <Description("Monthly grazing intervals")> _
    <Param()> <Output()> Public Property GrazingIntervalByMonth() As String
        '<Description("Monthly grazing intervals")> _
        '<Output()> Public Property GrazingIntervalByMonth() As String
        Get
            Dim b() As String = Array.ConvertAll(Of Integer, String)(dairyNZ_mg, New Converter(Of Integer, String)(AddressOf MyCStr))
            Dim c As String = String.Join(",", b)
            Return c
        End Get
        Set(ByVal value As String)
            dairyNZ_mg = StringtoIntegerArray(value)
        End Set
    End Property
    <Description("Monthly grazing residuals")> _
    <Param()> <Output()> Public Property GrazingResidualByMonth() As String
        '<Description("Monthly grazing residuals")> _
        '<Output()> Public Property GrazingResidualByMonth() As String
        Get
            Dim b() As String = Array.ConvertAll(Of Integer, String)(dairyNZ_gr, New Converter(Of Integer, String)(AddressOf MyCStr))
            Dim c As String = String.Join(",", b)
            Return c
        End Get
        Set(ByVal value As String)
            dairyNZ_gr = StringtoIntegerArray(value)
        End Set
    End Property
    <Description("Monthly grazing residuals (Val)")> _
        <Param()> <Output()> Public Property GrazingResidualValByMonth() As String
        '<Description("Monthly grazing residuals (Val)")> _
        '<Output()> Public Property GrazingResidualValByMonth() As String
        Get
            Dim b() As String = Array.ConvertAll(Of Integer, String)(Val_gr, New Converter(Of Integer, String)(AddressOf MyCStr))
            Dim c As String = String.Join(",", b)
            Return c
        End Get
        Set(ByVal value As String)
            Val_gr = StringtoIntegerArray(value)
        End Set
    End Property

    <Description("Control animal grazing to simulate break feeding of individual paddocks [0=No, 1=Yes]")> _
    <Param()> Property BreakFeeding() As Integer
        Get
            If PaddockWrapper.DebugTestBreakFeeding Then
                Return 1
            Else
                Return 0
            End If
        End Get
        Set(ByVal value As Integer)
            PaddockWrapper.DebugTestBreakFeeding = value > 0
        End Set
    End Property

    <Description("Switch to include stolon mass in calculations and reporting [0=No, 1=Yes]")> _
    <Param()> Property IncludeStolon() As Integer
        Get
            If PaddockWrapper.IncludeStolon Then
                Return 1
            Else
                Return 0
            End If
        End Get
        Set(ByVal value As Integer)
            PaddockWrapper.IncludeStolon = value > 0
        End Set
    End Property

    <Description("Testing: Use a moving average of daily growth to predict future growth based in past growth")> _
    <Param()> <Output()> Property GrowthRateWindowSize() As Integer
        Get
            Return PaddockWrapper.MovingAverageSeriesLength
        End Get
        Set(ByVal value As Integer)
            PaddockWrapper.MovingAverageSeriesLength = value
            Farm.MovingAverageSeriesLength = value
        End Set

    End Property
    <Description("Surplus cover")> _
     <Param()> <Output()> <Units("0-1")> Public Property SurplusCover() As Double
        '<Description("Surplus cover")> _
        '<Output()> <Units("0-1")> Public Property SurplusCover() As Double
        Get
            Return myFarm.Surplus
        End Get
        Set(ByVal value As Double)
            myFarm.Surplus = value
        End Set

    End Property
    <Description("Stocking rate threshold")> _
    <Param()> <Output()> <Units("0-1")> Public Property Stocking_Rate_Threshold__() As Double
        '<Description("Stocking rate threshold")> _
        '<Output()> <Units("0-1")> Public Property Stocking_Rate_Threshold__() As Double
        Get
            Return myFarm.Stocking_Rate_Threshold_
        End Get
        Set(ByVal value As Double)
            myFarm.Stocking_Rate_Threshold_ = value
        End Set
    End Property
    <Description("Pre-grazing cover target")> _
        <Param()> <Output()> Public Property PreGrazingCoverTarget__() As Double
        '<Description("Pre-grazing cover target")> _
        '<Output()> Public Property PreGrazingCoverTarget__() As Double
        Get
            Return myFarm.PreGrazingCoverTarget
        End Get
        Set(ByVal value As Double)
            myFarm.PreGrazingCoverTarget = value
        End Set
    End Property
    <Description("Max pre-grazing cover target")> _
       <Param()> <Output()> Public Property MaxPreGrazingCoverTarget__() As Double
        '<Description("Max pre-grazing cover target")> _
        '<Output()> Public Property MaxPreGrazingCoverTarget__() As Double
        Get
            Return myFarm.MaxPreGrazingCoverTarget
        End Get
        Set(ByVal value As Double)
            myFarm.MaxPreGrazingCoverTarget = value
        End Set
    End Property
    <Description("Simple cow live weight")> _
       <Param()> <Output()> Public Property Live_Weight__() As Double
        '<Description("Simple cow live weight")> _
        '<Output()> Public Property Live_Weight__() As Double
        Get
            Return Live_Weight
        End Get
        Set(ByVal value As Double)
            Live_Weight = value
        End Set
    End Property
    <Description("Simple cow condition score")> _
        <Param()> <Output()> Public Property ConditionScore__() As Double
        '<Description("Simple cow condition score")> _
        '<Output()> Public Property ConditionScore__() As Double
        Get
            Return ConditionScore
        End Get
        Set(ByVal value As Double)
            ConditionScore = value
        End Set
    End Property



    <Description("Proportion of farm to return effluent to")> _
    <Param()> <Output()> <Units("0-1")> Public Property EffluentPaddocksPercentage() As Double
        '<Description("Proportion of farm to return effluent to")> _
        '<Output()> <Units("0-1")> Public Property EffluentPaddocksPercentage() As Double
        Get
            Return myFarm.EffluentPaddocksPercentage
        End Get
        Set(ByVal value As Double)
            myFarm.EffluentPaddocksPercentage = value
        End Set

    End Property


    'WIP: Fire an "Graze" event when pasture is removed by DDRules
    '<[Event]()> Public Event Grazing1 As GrazeDelegate
    '<[Event]()> Public Event Grazing2 As AnimalGrazeDelegate

    'Private Sub PublishGrazingEvent()
    '        'option 1
    '        Dim grazeData As New GrazeType
    '        grazeData.amount = 1
    '        grazeData.type = "DairyCow"
    '        grazeData.sender = "DDRules"
    '        RaiseEvent Grazing1(grazeData)

    '        'option 2
    '        Dim grazeData2 As New AnimalGrazeType
    '        grazeData2.species = "DairyCow"
    '        grazeData2.stocking_rate = 2.5
    '        grazeData2.time_on_pasture = 24
    '        grazeData2.sender = "DDRules"
    '        RaiseEvent Grazing2(grazeData2)
    'End Sub

    ''Dummy event handler to test event above
    '<[EventHandler]()> Public Sub OnGrazing1(ByVal data As GrazeType)
    '        Console.WriteLine("DDRules (Testing) - Caught Graze1 event")
    'End Sub

    '<[EventHandler]()> Public Sub OnGrazing2(ByVal data As AnimalGrazeType)
    '        Console.WriteLine("DDRules (Testing) - Caught Graze2 event")
    'End Sub

    '<[EventHandler]()> Public Sub OnMove(ByVal data As MoveStockType)
    '        Console.WriteLine("DDRules (Testing) - Caught OnMove event")
    'End Sub

    ''       <[Event]()> Public Event SilageCut As PastureCutDelegate
    '<[EventHandler]()> Public Sub OnSilageCut(ByVal data As PastureCutType)
    '        Console.WriteLine("DDRules (OnSilageCut)")
    'End Sub

    'Private Sub PublishSilageEvent()
    '        Dim data As New PastureCutType
    '        data.cut_height = 1600
    '        data.dmd_loss = 0.1 '10% returned to SOM
    '        data.dm_content = 0 'not used
    '        data.gathered = 0 'not used
    '        RaiseEvent SilageCut(data)
    'End Sub


    <Description("Average pasture mass across the milking platform")> _
    <Output()> <Units("kg/ha")> Public ReadOnly Property AverageFarmCover() As Double
        Get
            Return myFarm.AverageCover()
        End Get
    End Property

    <Description("Average pasture mass across the milking platform")> _
    <Output()> <Units("kg/ha")> Public ReadOnly Property AverageFarmCoverWeekly() As Double
        Get
            Return myFarm.AverageFarmCoverWeekly()
        End Get
    End Property

    <Param()> <Output()> Public Property a_RPM__() As Double
        '<Output()> Public Property a_RPM__() As Double
        Get
            Return a
        End Get
        Set(ByVal value As Double)
            a = a_RPM__
        End Set
    End Property
    <Param()> <Output()> Public Property b_RPM__() As Double
        '<Output()> Public Property b_RPM__() As Double
        Get
            Return b
        End Get
        Set(ByVal value As Double)
            b = b_RPM__
        End Set
    End Property
    <Param()> <Output()> Public Property RotationInterval__() As Integer
        '<Output()> Public Property RotationInterval__() As Integer
        Get
            Return Rotation            '
        End Get
        Set(ByVal value As Integer)
            Rotation = RotationInterval__
        End Set
    End Property

    <Description("Average pasture mass across the milking platform in 'clicks'. Source 1-15 Using the Rising Plate Meter (RPM)")> _
    <Output()> <Units("kg/ha")> Public ReadOnly Property AverageCoverClicks() As Double
        Get
            'Return (myFarm.AverageCover() - 500) / 140.0
            Return (myFarm.AverageCover() - a_RPM__) / b_RPM__
        End Get
    End Property

    <Description("Amount of dry matter on farm in relation to the amount required to meet animal demands (+ve = surplus, -ve deficit)")> _
    <Output()> <Units("kg/ha")> Public ReadOnly Property FeedSituation() As Double
        Get
            Return myFarm.FeedSituation()
        End Get
    End Property

    <Description("Ideal average pasture mass across the milking platform in order to meet animal demands")> _
    <Output()> <Units("kg/ha")> Public ReadOnly Property IdealAvgGrazingCover() As Double
        Get
            Return myFarm.IdealGrazingCover()
        End Get
    End Property

    <Description("Ideal pre-grazing pasture mass required to meet animal demands, post-grazing residual and rotation length")> _
    <Output()> <Units("kg/ha")> Public ReadOnly Property IdealPreGrazingCover() As Double
        Get
            Return myFarm.IdealPreGrazingCover()
        End Get
    End Property

    <Description("Test array output (not used)")> _
    <Output()> <Units("")> Public Property Test_Pdks() As String()
        Get
            Return New String() {"This", "is", "a", "test"}
        End Get
        Set(ByVal value As String())
            For Each Str As String In value
                Console.Write(Str + " ")
            Next
            Console.WriteLine()
        End Set
    End Property

    '<Description("Average plant available water (proportion availble in the top 300mm)")> _
    '<Output()> <Units("0-1")> Public ReadOnly Property PlantAvalibleWater As Single
    '    Get
    '        Return myFarm.PlantAvalibleWater(300)
    '    End Get
    'End Property

    <Description("Set milk production data")> _
    <Param()> <Output()> <Units("kgMS/cow/day")> Public Property MilkCurve As String
        Get
            Return myFarm.getMilkSolids()
        End Get
        Set(ByVal value As String)
            Dim v As Double() = strToDoubleArray(value, 12)
            Dim values(v.Length - 1) As Double
            For i As Integer = 0 To v.Length - 1
                If (DebugLevel > 0) Then
                    Console.Write("Milk Curve = " + v(i).ToString())
                End If

                values(i) = v((i + 6) Mod 12)

                If (DebugLevel > 0) Then
                    Console.WriteLine(" [ha] = " + (v(i) * StockingRate).ToString("0.00"))
                End If
            Next
            'Dim strValues As String() = value 'value.Split(",".ToCharArray())
            ''    Dim values(11) As Double
            ''    For i As Integer = 0 To 11 'strValues.Length - 1
            ''        If (DebugLevel > 0) Then
            ''            Console.Write("Milk Curve = " + value(i).ToString())
            ''        End If
            ''        values(i) = value((i + 6) Mod 12)
            ''        If (DebugLevel > 0) Then
            ''            Console.WriteLine(" [ha] = " + (values(i) * StockingRate).ToString("0.00"))
            ''        End If
            ''    Next
            myFarm.setMilkSolids(values)
        End Set
    End Property

    <Description("Set paddocks to return dairyshed effluent to")> _
    <Param()> <Output()> <Units("")> Public Property EffluentPaddocks As String()
        Get
            If (strEffluentPaddocks Is Nothing) Then
                Return New String() {""}
            End If
            Return strEffluentPaddocks 'myFarm.getEffluentPaddocks()
        End Get
        Set(ByVal value As String())
            Dim strValues As String() = value 'value.Split(",".ToCharArray())
            ReDim strEffluentPaddocks(strValues.Length - 1)
            For i As Integer = 0 To strValues.Length - 1
                If (DebugLevel > 0) Then
                    Console.Write("EffluentPaddocks = " + strValues(i))
                End If
                strEffluentPaddocks(i) = strValues(i).TrimEnd(",".ToCharArray()).TrimStart(" ".ToCharArray())

                If (DebugLevel > 0) Then
                    Console.WriteLine(" = " + strEffluentPaddocks(i))
                End If
            Next
            'myFarm.setEffluentPaddocks(strEffluentPaddocks) have to do this later, set after farm is initialised
        End Set
    End Property

    <Description("Set paddocks to return laneway effluent to")> _
    <Output()> <Units("")> Public Property LanewayPaddocks As String
        Get
            If (strLanewayPaddocks Is Nothing) Then
                Return ""
            End If
            Dim result As String = ""
            For Each str As String In strLanewayPaddocks
                result += str + ","
            Next
            If (result.Length > 0) Then
                Return result.Substring(0, result.Length - 1)
            End If
            Return result
        End Get
        Set(ByVal value As String)
            Dim strValues As String() = value.Split(",".ToCharArray())
            ReDim strLanewayPaddocks(strValues.Length - 1)
            For i As Integer = 0 To strValues.Length - 1
                If (DebugLevel > 0) Then
                    Console.Write("Laneway Paddocks = " + strValues(i))
                End If
                strLanewayPaddocks(i) = strValues(i).TrimEnd
                If (DebugLevel > 0) Then
                    Console.WriteLine(" = " + strLanewayPaddocks(i))
                End If
            Next
        End Set
    End Property
    Private Function strToDoubleArray(ByVal s As String, ByVal l As Integer) As Double()
        Dim values(l - 1) As Double
        Dim strValues() As String = s.Split(",".ToCharArray())
        For i As Integer = 0 To l - 1
            values(i) = CDbl(strValues(i))
        Next
        Return values
    End Function
    <Description("Set Live Weight Profile")> _
    <Param()> <Output()> <Units("kg")> Public Property LWtCurve As String
        Get
            Return myFarm.getLiveWeight()
        End Get
        Set(ByVal value As String)
            Dim v As Double() = strToDoubleArray(value, 12)
            Dim values(11) As Double
            For i As Integer = 0 To 11 'strValues.Length - 1
                values(i) = v((i + 6) Mod 12)
            Next
            myFarm.setLiveWeight(values)
        End Set
    End Property

    <Description("Grazing allocation system used (0=Simple, 1=Feed Wedge)")> _
    <Output()> <Units("0,1")> Public Property AllocationType() As Integer
        Get
            Return myFarm.AllocationType
        End Get
        Set(ByVal value As Integer)
            myFarm.AllocationType = value
        End Set
    End Property

    <Description("Proportion of total cows to dry off [0-1]")> _
       <Param()> <Output()> <Units("0-1")> Public Property DryOffProportion() As Double
        Get
            Return myFarm.DryOffProportion
        End Get
        Set(ByVal value As Double)
            myFarm.DryOffProportion = value
        End Set
    End Property

    <Description("Default pasture ME")> _
    <Param()> <Output()> Public Property DefaultPastureMetEnergy() As String
        '<Description("Default pasture ME")> _
        '<Output()> Public Property DefaultPastureMetEnergy() As String
        Get
            Return myFarm.DefaultPastureMetEnergy
        End Get
        Set(ByVal value As String)
            myFarm.DefaultPastureMetEnergy = value
        End Set
    End Property

    <Description("Number of dry cows on farm")> _
    <Output()> ReadOnly Property DryCows As Double
        Get
            Return myFarm.DryCows
        End Get
    End Property

    <Description("Number of milking cows on farm")> _
    <Output()> ReadOnly Property MilkingCows As Double
        Get
            Return myFarm.MilkingCows
        End Get
    End Property

    'Notes: Nitrogen Balance
    'Inputs
    '       Fert events
    '       Urine returns
    '       Dung returns
    '       Liberates through weight loss (?)
    '       purchased feed
    '       Clover fixation
    '       Root turnover?
    'Outputs
    '       Leached
    '       Volatalised
    '       Removed in milk
    '       Sequestered in body weight
    '       Silage stored
End Class
