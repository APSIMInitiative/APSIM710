Imports System
Imports System.Collections.Generic
Imports System.Text
Imports ModelFramework
Imports System.Xml
Imports System.Xml.Schema
Imports CSGeneral

'20110523 - Convert all internals to function on a per hectare basic [issues with scaling somewhere in here


'Bugs:
'    Rotation lenghts not being respected. Need to implement the day count type method or fix paddock status
'    Grazing counter can reach -1

' Why is it crashing (-1 error)
' Why does the .Varabable request cause a fatal error
' Why doesn't the summarry file get updated like the console output

' Apsim Type applicable to DDRules
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
'       post 1480 = 7 clicks
'       pre 3100-3200
'       average 2300-2400
'       20 day during growth
'       out to 35 days at drying off
'       Paddocks over pre-graze taken out and cut imediatly


Public Class DDRules
        Inherits Instance
        Public myDebugLevel As Integer = 0
        <Link()> Private MySimulation As Paddock
        Private myFarm As Farm
        Private myHerd As SimpleHerd 'local handle to the herd contained in Farm. This is only a short term fix

        <Input()> Private month As Integer
        <Input()> Private year As Integer
        <Input()> Private day_of_month As Integer
        <Input()> Private end_week As Integer
        <Input()> Private Start_week As Integer

        Public dairyNZ_mg As Single() = {20, 25, 30, 40, 50, 100, 100, 80, 50, 25, 20, 20}  'jan to dec
        Public dairyNZ_gr As Single() = {1600, 1600, 1600, 1500, 1400, 1200, 1200, 1400, 1500, 1500, 1500, 1500}  'june to may
        Public Val_gr As Single() = {1600, 1600, 1600, 1600, 1600, 1200, 1200, 1600, 1600, 1600, 1600, 1600}  'june to may - altered by Val for FarmSim
        Public default_mg As Single() = dairyNZ_mg
        Public default_gr As Single() = dairyNZ_gr

        Public Sub New()
                myFarm = New Farm()
        End Sub

#Region "EventHandlers"
        'Initialise farm
        <EventHandler()> Public Sub OnInit1()
                DebugLevel = myDebugLevel
                If (DebugLevel > 0) Then
                        Console.WriteLine("Enter OnInit1()")
                End If
        End Sub
        <EventHandler()> Public Sub OnInit2()
                DebugLevel = myDebugLevel
                If (DebugLevel > 0) Then
                        Console.WriteLine("Enter OnInit2()")
                End If

                ' ************* Farm testing **********************
                If (TotalFarmArea <= 0) Then
                        TotalFarmArea = MySimulation.SubPaddocks.Count 'default to one hectare paddocks if no area set
                End If

                myFarm.Init(MySimulation, year, month, TotalFarmArea)
                myHerd = myFarm.getHerd()

                SetupFarmSim(MySimulation)

                PrintFarmSummary()

                GrazingIntervalIsSet = False
                GrazingResidualIsSet = False
                'PaddockGrazable(0) = 0 'testing removal of a paddock from the rotation i.e. for forage crops etc.
                'PaddockGrazable(2) = 0 'testing removal of a paddock from the rotation i.e. for forage crops etc.
                myIrrigation_efficiency = 1.0
        End Sub

        'Reset tracked output variables
        <EventHandler()> Sub OnNewMet()
                myIrrigationAmount = 0
                myFertiliserAmount = 0
        End Sub

        'Reset paddock status etc. 
        <EventHandler()> Sub OnPrepare()
                If (DebugLevel > 0) Then
                        Console.WriteLine("Enter OnPrepare()")
                End If

                'Todo: find a way to default to the dairy nz parameter if the user has not entered anything
                ' If I introduce the line below they overwrite any values set by a manager component
                ' Initilising "flag" values here to trigger default setting during OnProcess
                '        GrazingInterval = dairyNZ_mg(month - 1) 'Minium Grazing Interval (actually rotation length)
                '        GrazingResidual = dairyNZ_gr(month - 1) 'Grazing residual
                'GrazingIntervalIsSet = False
                'GrazingResidualIsSet = False

                ' ************* Farm testing **********************
                myFarm.Prepare(year, month, day_of_month, end_week)
                myFarm.StockingRate = BaseStockingRate
                ' ************* Farm testing **********************

                If (DebugLevel > 0) Then
                        Console.WriteLine("   Rotation Length " & GrazingInterval.ToString)
                        Console.WriteLine("   Residual " & GrazingResidual.ToString)
                        Console.WriteLine("   Stocking Rate " & StockingRate.ToString)
                End If
        End Sub

        'Main model step
        <EventHandler()> Sub OnProcess()
                ' ************* Farm testing **********************
                If Not (GrazingIntervalIsSet) Then
                        GrazingInterval = dairyNZ_mg(month - 1)
                        GrazingIntervalIsSet = False 'not user set so reset switch
                End If
                If Not (GrazingResidualIsSet) Then
                        GrazingResidual = dairyNZ_gr(month - 1)
                        GrazingResidualIsSet = False 'not user set so reset switch
                End If

                myFarm.GrazingResidual = GrazingResidual
                myFarm.GrazingInterval = GrazingInterval
                'myFarm.StockingRate = sr
                myFarm.Process(Start_week)

                Dim cover = myFarm.AverageCover
                ' ************* Farm testing **********************
                PrepareOutputs()

                'PublishGrazingEvent() - testing
        End Sub

        '<EventHandler()> Sub Onremove_crop_biomass()
        '        'this doesn't work!
        '        Console.WriteLine("DDRules heard a remove_crop_biomass event")
        'End Sub

        '<EventHandler()> Sub OnBiomassRemoved()
        '        'this works
        '        'Console.WriteLine("DDRules heard a BiomassRemoved event")
        'End Sub

        'Whole farm fertiliser application
        <EventHandler()> Public Sub OnApplyFertiliser(ByVal amount As FertiliserApplicationType)
                Console.WriteLine("   myFertiliserAmount  = " + myFertiliserAmount.ToString())
                myFertiliserAmount += myFarm.Fertilise(amount)
                Console.WriteLine("   myFertiliserAmount  = " + myFertiliserAmount.ToString())
        End Sub

        Dim myIrrigationAmount As Double = 0
        Dim myFertiliserAmount As Double = 0

        'Whole farm irrigation application
        <EventHandler()> Public Sub OnApplyIrrigation(ByVal amount As IrrigationApplicationType)
                Console.WriteLine("   myIrrigationAmount  = " + myIrrigationAmount.ToString())
                myIrrigationAmount += myFarm.Irrigate(amount)
                Console.WriteLine("   myIrrigationAmount  = " + myIrrigationAmount.ToString())
        End Sub

        'Testing overloaded irrigation Apply event
        <EventHandler()> Public Sub OnApply(ByVal amount As IrrigationApplicationType)
                '     Dim t As ApsimType = amount
                '    If (TypeOf t Is IrrigationApplicationType) Then
                Console.WriteLine("   Irrigation Event triggered")
                '                OnApplyIrrigation(amount)
        End Sub

        'Testing overloaded fertiliser Apply event
        <EventHandler()> Public Sub OnApply(ByVal amount As FertiliserApplicationType)
                '                Console.WriteLine("   Fertiliser Event triggered")
                '                OnApplyFert(amount)
        End Sub

        'Amount of water applied in relation to the whole farm
        <Output()> <Units("mm/ha")> Public ReadOnly Property Irrigation() As Double
                Get
                        Return myIrrigationAmount
                End Get
        End Property

        'Amount of fertiliser applied in relation to the whole farm
        <Output()> <Units("kg/ha")> Public ReadOnly Property Fertiliser() As Double
                Get
                        Return myFertiliserAmount
                End Get
        End Property

#End Region

        Private TotalFarmArea As Double = 0
        'Effective farm area [ha]
        <Output()> <Units("ha")> Public Property FarmArea() As Double
                Get
                        Return TotalFarmArea
                End Get
                Set(ByVal value As Double)
                        TotalFarmArea = value
                End Set
        End Property

        'Take dry stock off farm
        <Output()> <Units("")> Public Property WinterOffDryStock() As Integer
                Get
                        If (myFarm.WinterOffDryStock) Then
                                Return 1
                        Else
                                Return 0
                        End If
                End Get
                Set(ByVal value As Integer)
                        myFarm.WinterOffDryStock = value > 0
                End Set
        End Property

        'Set paddocks avalibility to be included in the grazing rotation
        <Output()> <Units("")> Public Property PaddockGrazable(ByVal i As Integer) As Integer
                Get
                        Return myFarm.PaddockGrazable(i)
                End Get
                Set(ByVal value As Integer)
                        myFarm.PaddockGrazable(i) = value
                End Set
        End Property

        Sub SetupFarmSim(ByVal MyPaddock As Paddock)
                Dim FarmSim As Component = MyPaddock.ComponentByType("FarmSimGraze")
                If FarmSim Is Nothing Then
                        Return
                End If

                Dim UI_StockRate As Double = FarmSim.Variable("UI_StockRate").ToDouble
                Dim UI_SuppType As String = FarmSim.Variable("UI_SuppType").ToString

                BaseStockingRate = UI_StockRate
                StockingRate = BaseStockingRate

                Dim GrassSilage As Integer = 0
                Dim GrainOrConcentrate As Integer = 1
                If (UI_SuppType = "GrassSilage") Then
                        myFarm.SupplementME = 10
                        myFarm.SupplementN = 0.035
                        myFarm.SupplementWastage = 0.0
                        myFarm.SupplementDigestability = 0.7 ' minimum for high quality silage. Source: DairyNZ FarmFact 1-46
                Else 'GrainOrConcentrate
                        myFarm.SupplementME = 12
                        myFarm.SupplementN = 0.018
                        myFarm.SupplementWastage = 0.0
                        myFarm.SupplementDigestability = 0.8
                End If

                myFarm.EnableSilageStore = False 'all supplement purchase / no silage kept
                WinterOffDryStock = True 'all stock wintered off farm
                default_gr = Val_gr
        End Sub


#Region "CowProperties"
        'Stocking is the actual number of cows on farm i.e. normal stocking rate less cows wintering off
        <Output()> <Units("cows/ha")> Public Property StockingRate() As Single
                Get
                        Return myFarm.StockingRate
                End Get
                Set(ByVal value As Single)
                        If (value >= 0) Then
                                myFarm.StockingRate = value
                        End If
                End Set
        End Property

        Private BaseStockingRate As Double = 2.5 'default value
        'Stocking rate a peak of lactation
        <Output()> <Units("cows/ha")> Public Property PeakStockingRate() As Single
                Get
                        Return BaseStockingRate
                End Get
                Set(ByVal value As Single)
                        If (value >= 0) Then
                                BaseStockingRate = value
                        Else
                                BaseStockingRate = 0
                        End If
                End Set
        End Property

        'ME still required to meet animal requirements
        <Output()> <Units("MJME")> Public ReadOnly Property RemainingFeedDemand() As Double
                Get
                        Return myHerd.RemainingFeedDemand
                End Get
        End Property

        'Total animal requirements [MJME] (for the full herd)
        <Output()> <Units("Cows")> Public ReadOnly Property TotalCows() As Double
                Get
                        Return myHerd.Number_Of_Cows
                End Get
        End Property

        'Total animal requirements [MJME] (for the full herd)
        <Output()> <Units("MJME")> Public ReadOnly Property TotalFeedDemand() As Double
                Get
                        Return myHerd.TodaysEnergyRequirement
                End Get
        End Property

#Region "ME Requirements (Herd)"
        'Total Metabolisable energy required by herd [MJME/day]
        <Output()> <Units("MJME")> Public ReadOnly Property ME_Demand_Total() As Double
                Get
                        Return myHerd.ME_Total
                End Get
        End Property

        'Metabolisable energy required by herd for maintance [MJME/day]
        <Output()> <Units("MJME")> Public ReadOnly Property ME_Demand_Maintance() As Double
                Get
                        Return myHerd.ME_Maintance
                End Get
        End Property

        'Metabolisable energy required by herd for lactation [MJME/day]
        <Output()> <Units("MJME")> Public ReadOnly Property ME_Demand_Lactation() As Double
                Get
                        Return myHerd.ME_Lactation
                End Get
        End Property
        'Metabolisable energy required by herd for pregnancy [MJME/day]
        <Output()> <Units("MJME")> Public ReadOnly Property ME_Demand_Pregnancy() As Double
                Get
                        Return myHerd.ME_Pregnancy
                End Get
        End Property
        ' Metabolisable energy required by herd for walking [MJME/day]
        <Output()> <Units("MJME")> Public ReadOnly Property ME_Demand_Walking() As Double
                Get
                        Return myHerd.ME_Walking
                End Get
        End Property
        'Metabolisable energy required by herd for live weight change [MJME/day]
        <Output()> <Units("MJME")> Public ReadOnly Property ME_Demand_WeightChange() As Double
                Get
                        Return myHerd.ME_WeightChange
                End Get
        End Property
#End Region
#Region "ME Requirements (Cow)"
        'Total Metabolisable energy required [MJME/cow/day]
        <Output()> <Units("MJME/cow")> Public ReadOnly Property ME_Demand_Total_Cow() As Double
                Get
                        Return myHerd.ME_Total_Cow
                End Get
        End Property

        'Metabolisable energy required for maintainence [MJME/cow/day]
        <Output()> <Units("MJME/cow")> Public ReadOnly Property ME_Demand_Maintance_Cow() As Double
                Get
                        Return myHerd.ME_Maintance_Cow
                End Get
        End Property
        'Metabolisable energy required for lactation [MJME/cow/day]
        <Output()> <Units("MJME/cow")> Public ReadOnly Property ME_Demand_Lactation_Cow() As Double
                Get
                        Return myHerd.ME_Lactation_Cow
                End Get
        End Property
        'Metabolisable energy required for live weight change [MJME/cow/day]
        <Output()> <Units("MJME/cow")> Public ReadOnly Property ME_Demand_WeightChange_Cow() As Double
                Get
                        Return myHerd.ME_WeightChange_Cow
                End Get
        End Property
        'Metabolisable energy required for pregnancy [MJME/cow/day]
        <Output()> <Units("MJME/cow")> Public ReadOnly Property ME_Demand_Pregnancy_Cow() As Double
                Get
                        Return myHerd.ME_Pregnancy_Cow
                End Get
        End Property
        'Metabolisable energy required for walking [MJME/cow/day]
        <Output()> <Units("MJME/cow")> Public ReadOnly Property ME_Demand_Walking_Cow() As Double
                Get
                        Return myHerd.ME_Walking_Cow
                End Get
        End Property
#End Region
        'Milk solids production - total
        <Output()> <Units("kgMS/day")> Public ReadOnly Property MilkSolids() As Double
                Get
                        Return myHerd.MS_per_Day
                End Get
        End Property

        'Milk solids production - total
        <Output()> <Units("kgMS/day")> Public ReadOnly Property MilkSolids_Ha() As Double
                Get
                        Return myHerd.MS_per_Day_Cow * StockingRate
                End Get
        End Property

        'Milk solids production per cow
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

        'Average cow live weight
        <Output()> <Units("kg_LWt/Cow")> Public ReadOnly Property Cow_LWt() As Double
                Get
                        Return myHerd.Live_Weight
                End Get
        End Property

        'Average cows change in live weight
        <Output()> <Units("kg_LWt/cow/day")> Public ReadOnly Property Cow_dLWt() As Double
                Get
                        Return myHerd.LWt_Change
                End Get
        End Property

        'Averge body condition score
        <Output()> <Units("")> Public ReadOnly Property Cow_BC() As Double
                Get
                        Return myHerd.BC
                End Get
        End Property

        'Is herd currently dried off
        <Output()> <Units("")> Public ReadOnly Property Cow_IsDry() As String
                Get
                        Return myHerd.isDry.ToString
                End Get
        End Property
#End Region

        Private GrazingIntervalIsSet As Boolean = False
        Private GrazingResidualIsSet As Boolean = False

        'Current grazing interval / return period
        <Output()> <Units("Days")> Public Property GrazingInterval() As Integer
                Get
                        Return myFarm.GrazingInterval
                End Get
                Set(ByVal value As Integer)
                        GrazingIntervalIsSet = True
                        myFarm.GrazingInterval = value
                End Set
        End Property

        'Current cow grazing residual
        <Output()> <Units("kgDM/ha")> Public Property GrazingResidual() As Integer
                Get
                        Return myFarm.GrazingResidual
                End Get
                Set(ByVal value As Integer)
                        GrazingResidualIsSet = True
                        myFarm.GrazingResidual = value
                End Set
        End Property

        'Quantity of silage cut on farm today [kgDM/day]
        <Output()> <Units("kgDM")> Public ReadOnly Property SilageCut_kg() As Double
                Get
                        Return myFarm.SilageCut
                End Get
        End Property

        'Quantity of silage cut on farm today [kgDM/ha/day]
        <Output()> <Units("kgDM/ha")> Public ReadOnly Property SilageCut_kgha() As Double
                Get
                        Return SilageCut_kg / FarmArea
                End Get
        End Property

#Region "2: Feeding Supplements"

        'Quantity of silage fed to cows today [kgDM/ha/day]
        <Output()> <Units("kgDM/ha")> Public ReadOnly Property SupplementFedOut_kgha() As Double
                Get
                        Return SupplementFedOut / FarmArea
                End Get
        End Property

        'Quantity of silage fed to cows today [kgDM/ha/day]
        <Output()> <Units("kgDM/ha")> Public ReadOnly Property SilageFedOut_kgha() As Double
                Get
                        Return SilageFedOut / FarmArea
                End Get
        End Property

        'Quantity of silage fed to cows today [kgDM/day]
        <Output()> <Units("kgDM")> Public ReadOnly Property SupplementFedOut() As Double
                Get
                        Return myFarm.SupplementFedOut
                End Get
        End Property

        'Quantity of silage fed to cows today [kgDM/day]
        <Output()> <Units("kgDM")> Public ReadOnly Property SilageFedOut() As Double
                Get
                        Return myFarm.SilageFedOut
                End Get
        End Property

        'Energy content of purchased supplement
        <Output()> <Units("MJME/kgDM")> Public Property SupplementME() As Double
                Get
                        Return myFarm.SupplementME
                End Get
                Set(ByVal value As Double)
                        myFarm.SupplementME = value
                End Set
        End Property

        'Nitrogen content of purchased supplement
        <Output()> <Units("kgN/kgDM")> Public Property SupplementN() As Double
                Get
                        Return myFarm.SupplementN
                End Get
                Set(ByVal value As Double)
                        myFarm.SupplementN = value
                End Set
        End Property

        'Supplment digestability
        <Output()> <Units("0-1")> Public Property SupplementDigestability() As Double
                Get
                        Return myFarm.SupplementDigestability
                End Get
                Set(ByVal value As Double)
                        myFarm.SupplementDigestability = value
                End Set
        End Property

        'Supplment loss at feeding out
        <Output()> <Units("%")> Public Property SupplementWastage() As Double
                Get
                        Return myFarm.SupplementWastage * 100
                End Get
                Set(ByVal value As Double)
                        myFarm.SupplementWastage = value / 100
                End Set
        End Property

        'Proportion of silage lost during feeding out
        <Output()> <Units("0-1")> Public Property SilageWastage() As Double
                Get
                        Return myFarm.SilageWastage * 100
                End Get
                Set(ByVal value As Double)
                        myFarm.SilageWastage = value / 100
                End Set
        End Property

        'Silage digestability (could this be made read only once the store has been refactored)
        <Output()> <Units("0-1")> Public Property SilageDigestability() As Double
                Get
                        Return myFarm.SilageDigestability
                End Get
                Set(ByVal value As Double)
                        myFarm.SilageDigestability = value
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

        'Energy content of silage produced on farm
        <Output()> <Units("MJME/kgDM")> Public Property SilageME() As Double
                Get
                        Return myFarm.SilageME
                End Get
                Set(ByVal value As Double)
                        myFarm.SilageME = value
                End Set
        End Property

        'Nitrogen content of silage produced on farm
        <Output()> <Units("kgN/kgDM")> Public Property SilageN() As Double
                Get
                        Return myFarm.SilageN
                End Get
                Set(ByVal value As Double)
                        myFarm.SilageN = value
                End Set
        End Property

        'Supplment/silage loss during cutting
        <Output()> <Units("%")> Public Property SilageCutWastage() As Double
                Get
                        Return myFarm.SilageCutWastage * 100
                End Get
                Set(ByVal value As Double)
                        myFarm.SilageCutWastage = value / 100
                End Set
        End Property
        'First Conservation Date
        <Output()> <Units("")> Public Property ConservationStart() As String
                Get
                        Return myFarm.FCD
                End Get
                Set(ByVal value As String)
                        myFarm.FCD = Date.Parse(value)
                End Set
        End Property

        'Last Conservation Date
        <Output()> <Units("")> Public Property ConservationFinish() As String
                Get
                        Return myFarm.LCD
                End Get
                Set(ByVal value As String)
                        myFarm.LCD = Date.Parse(value)
                End Set
        End Property

        'Storage of cut silage on farm (for later use)
        <Output()> <Units("")> Public Property SilageStoreEnable() As Integer
                Get
                        If myFarm.EnableSilageStore Then
                                Return 1
                        Else
                                Return 0

                        End If
                End Get
                Set(ByVal value As Integer)
                        myFarm.EnableSilageStore = value
                End Set
        End Property

        'Amount of silage avalible for feeding out [kgDM]
        <Output()> <Units("kgDM")> Public Property SilageStore() As Double
                Get
                        Return myFarm.SilageStore
                End Get
                Set(ByVal value As Double)
                        myFarm.SilageStore = value
                End Set
        End Property

        'Amount of silage avalible for feeding out [kgDM]
        <Output()> <Units("kgDM/ha")> Public Property SilageStore_kgha() As Double
                Get
                        Return SilageStore() / FarmArea()
                End Get
                Set(ByVal value As Double)
                        SilageStore = value / FarmArea()
                End Set
        End Property

        'Conservation trigger mass (close paddocks above this residual)
        <Output()> <Units("kgDM/ha")> Public Property ConservationTrigger() As Double
                Get
                        Return myFarm.CDM
                End Get
                Set(ByVal value As Double)
                        myFarm.CDM = value
                End Set
        End Property

        'Conservation cutting residual
        <Output()> <Units("kgDM/ha")> Public Property ConservationResidual() As Double
                Get
                        Return myFarm.CR
                End Get
                Set(ByVal value As Double)
                        myFarm.CR = value
                End Set
        End Property
#End Region

#Region "Additional Output Variables"
        Private Sub PrepareOutputs()
                myFarm.PrepareOutputs()

                DM_Eaten = myFarm.DM_Eaten
                DM_Eaten_Pasture = myFarm.DM_Eaten_Pasture
                DM_Eaten_Silage = myFarm.DM_Eaten_Silage
                DM_Eaten_Supplement = myFarm.DM_Eaten_Supplement
                ME_Demand = myFarm.ME_Demand
                ME_Eaten = myFarm.ME_Eaten
                ME_Eaten_Pasture = myFarm.ME_Eaten_Pasture
                ME_Eaten_Silage = myFarm.ME_Eaten_Silage
                ME_Eaten_Supplement = myFarm.ME_Eaten_Supplement
                N_Eaten = myFarm.N_Eaten
                N_Eaten_Pasture = myFarm.N_Eaten_Pasture
                N_Eaten_Supplement = myFarm.N_Eaten_Supplement
                N_to_milk = myFarm.N_to_milk
                N_to_BC = myFarm.N_to_BC
                N_to_feaces = myFarm.N_to_feaces
                DM_to_feaces = myFarm.DM_to_feaces
                N_to_urine = myFarm.N_to_urine
                N_Balance = myFarm.N_Balance
                N_Out = myFarm.N_Out

                ME_Demand_Cow = myHerd.ME_Demand_Cow()
                ME_Eaten_Cow = myHerd.ME_Eaten_Cow()
                ME_Eaten_Pasture_Cow = myHerd.ME_Eaten_Pasture_Cow()
                ME_Eaten_Supplement_Cow = myHerd.ME_Eaten_Supplement_Cow()
                DM_Eaten_Cow = myHerd.DM_Eaten_Cow()
                DM_Eaten_Pasture_Cow = myHerd.DM_Eaten_Pasture_Cow()
                DM_Eaten_Supplement_Cow = myHerd.DM_Eaten_Supplement_Cow()
                N_Eaten_Cow = myHerd.N_Eaten_Cow()
                N_Eaten_Pasture_Cow = myHerd.N_Eaten_Pasture_Cow()
                N_Eaten_Supplement_Cow = myHerd.N_Eaten_Supplement_Cow()
                N_to_milk_Cow = myHerd.N_to_milk_Cow()
                N_to_BC_Cow = myHerd.N_to_BC_Cow()
                N_to_feaces_Cow = myHerd.N_to_feaces_Cow()
                N_to_urine_Cow = myHerd.N_to_urine_Cow()
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

        'Total nitrogen balance of herd
        <Output()> <Units("kgN/ha")> Public N_Balance As Single
        'Nitrogen output of herd
        <Output()> <Units("kgN/ha")> Public N_Out As Single
        'Energy demand
        <Output()> <Units("MJME/ha")> Public ME_Demand As Single
        'Total Energy consumed
        <Output()> <Units("MJME/ha")> Public ME_Eaten As Single
        'Energy consumed as silage
        <Output()> <Units("MJME/ha")> Public ME_Eaten_Pasture As Single
        'Energy consumed as pasture
        <Output()> <Units("MJME/ha")> Public ME_Eaten_Silage As Single
        'Energy consumed as supplement
        <Output()> <Units("MJME/ha")> Public ME_Eaten_Supplement As Single
        'Total dry matter consumed
        <Output()> <Units("kgDM/ha")> Public DM_Eaten As Single
        'Dry matter consumed as pasture
        <Output()> <Units("kgDM/ha")> Public DM_Eaten_Pasture As Single
        'Dry matter consumed as silage
        <Output()> <Units("kgDM/ha")> Public DM_Eaten_Silage As Single
        'Dry matter consumed as supplement
        <Output()> <Units("kgDM/ha")> Public DM_Eaten_Supplement As Single
        'Total nitrogen consumed
        <Output()> <Units("kgN/ha")> Public N_Eaten As Single
        'Nitrogen consumed form pasture
        <Output()> <Units("kgN/ha")> Public N_Eaten_Pasture As Single
        'Nitrogen consumed form supplement
        <Output()> <Units("kgN/ha")> Public N_Eaten_Supplement As Single
        'Nitrogen partitioned to milk
        <Output()> <Units("kgN/ha")> Public N_to_milk As Single
        'Nitrogen partitioned to body condition
        <Output()> <Units("kgN/ha")> Public N_to_BC As Single
        'Nitrogen partitioned to dung
        <Output()> <Units("kgN/ha")> Public N_to_feaces As Single
        'Nitrogen partitioned to urine
        <Output()> <Units("kgN/ha")> Public N_to_urine As Single
        'Drymatter partitioned to dung
        <Output()> <Units("kgDM/ha")> Public DM_to_feaces As Single

        'Current status of paddocks
        <Output()> <Units("")> Public ReadOnly Property PaddockStatus() As String()
                Get
                        Return myFarm.PaddockStatus
                End Get
        End Property

        'Energy consumed as pasture per paddock
        <Output()> <Units("kgDM/ha/day")> Public ReadOnly Property AverageGrowthRate() As Single()
                Get
                        Return myFarm.AverageGrowthRate
                End Get
        End Property

        'Energy consumed as pasture per paddock
        <Output()> <Units("MJME/ha")> Public ReadOnly Property ME_Eaten_Pasture_Pdks() As Single()
                Get
                        Return myFarm.ME_Eaten_Pasture_Pdks
                End Get
        End Property
        'Energy consumed as Supplements per paddock
        <Output()> <Units("MJME/ha")> Public ME_Eaten_Supplement_Pdks As Single()

        'Energy consumed per paddock
        <Output()> <Units("MJME/ha")> Public ReadOnly Property ME_Eaten_Pdks() As Single()
                Get
                        Return myFarm.ME_Eaten_Pdks
                End Get
        End Property
        'Total Dry matter consumed per paddock [kgDM/day]
        <Output()> <Units("kgDM")> Public ReadOnly Property DM_Eaten_Pdks() As Single()
                Get
                        Return myFarm.DM_Eaten_Pdks
                End Get
        End Property

        'Dry matter consumed per paddock [kgDM/ha/day]
        <Output()> <Units("kgDM/ha")> Public ReadOnly Property DM_Eaten_Pdks_ha() As Single()
                Get
                        Return myFarm.DM_Eaten_Pdks_Ha
                End Get
        End Property

        'Dry matter consumed as pasture per paddock
        <Output()> <Units("kgDM/ha")> Public DM_Eaten_Pasture_Pdks As Single()
        'Dry matter consumed as supplement per paddock
        <Output()> <Units("kgDM/ha")> Public DM_Eaten_Supplement_Pdks As Single()
        'Nitrogen consumed per paddock
        <Output()> <Units("kgN/ha")> Public ReadOnly Property N_Eaten_Pdks() As Single()
                Get
                        Return myFarm.N_Eaten_Pdks
                End Get
        End Property
        'Nitrogen consumed as pasture per paddock
        <Output()> <Units("kgN/ha")> Public N_Eaten_Pasture_Pdks As Single()
        'Nitrogen consumed as supplement per paddock
        <Output()> <Units("kgN/ha")> Public N_Eaten_Supplement_Pdks As Single()
        'Nitrogen partitioned to milk per paddock
        <Output()> <Units("kgN/ha")> Public N_to_milk_Pdks As Single()
        'Nitrogen partitioned to body contition per paddock
        <Output()> <Units("kgN/ha")> Public N_to_BC_Pdks As Single()
        'Nitrogen partitioned to dung per paddock
        <Output()> <Units("kgN/ha")> Public ReadOnly Property N_to_feaces_Pdks() As Single()
                Get
                        Return myFarm.N_to_feaces_Pdks
                End Get
        End Property
        'Nitrogen partitioned to urine per paddock
        <Output()> <Units("kgN/ha")> Public ReadOnly Property N_to_urine_Pdks() As Single()
                Get
                        Return myFarm.N_to_urine_Pdks
                End Get
        End Property
        'Energy demand per cow
        <Output()> <Units("MJME/cow")> Public ME_Demand_Cow As Single
        'Energy consumed per cow
        <Output()> <Units("MJME/cow")> Public ME_Eaten_Cow As Single
        'Energy consumed as pasture per cow
        <Output()> <Units("MJME/cow")> Public ME_Eaten_Pasture_Cow As Single
        'Energy consumed as supplement per cow
        <Output()> <Units("MJME/cow")> Public ME_Eaten_Supplement_Cow As Single
        'Dry matter consumed per cow
        <Output()> <Units("kgDM/cow")> Public DM_Eaten_Cow As Single
        'Dry matter consumed as pasture per cow
        <Output()> <Units("kgDM/cow")> Public DM_Eaten_Pasture_Cow As Single
        'Dry matter consumed as supplement per cow
        <Output()> <Units("kgDM/cow")> Public DM_Eaten_Supplement_Cow As Single
        'Nitrogen consumed per cow
        <Output()> <Units("kgN/cow")> Public N_Eaten_Cow As Single
        'Nitrogen consumed as pasture per cow
        <Output()> <Units("kgN/cow")> Public N_Eaten_Pasture_Cow As Single
        'Nitrogen consumed as supplement per cow
        <Output()> <Units("kgN/cow")> Public N_Eaten_Supplement_Cow As Single
        'Nitrogen partitioned to milk per cow
        <Output()> <Units("kgN/cow")> Public N_to_milk_Cow As Single
        'Nitrogen partitioned to body condition per cow
        <Output()> <Units("kgN/cow")> Public N_to_BC_Cow As Single
        'Nitrogen partitioned to dung per cow
        <Output()> <Units("kgN/cow")> Public N_to_feaces_Cow As Single
        'Nitrogen partitioned to urine per cow
        <Output()> <Units("kgN/cow")> Public N_to_urine_Cow As Single
        'Daily per cow live weight change
#End Region

#Region "4. Management Options"
#Region "4a. Wintering Off Farm"
        ' two options
        ' 1) winter off dry stock
        ' 2) winter off between specific dates
        'Proportion (PWO) of herd wintered off-farm (range 0-1.0)
        <Output()> <Units("0-1")> Public Property PWO() As Single
                Get
                        Return myFarm.PWO
                End Get
                Set(ByVal value As Single)
                        myFarm.PWO = value
                End Set
        End Property
        ' Date commence winter off
        <Output()> <Units("")> Public Property DCWO() As String
                Get
                        Return myFarm.DCWO
                End Get
                Set(ByVal value As String)
                        myFarm.DCWO = Date.Parse(value)
                End Set
        End Property
        ' Date to stop winter off
        <Output()> <Units("")> Public Property DSWO() As String
                Get
                        Return myFarm.DSWO
                End Get
                Set(ByVal value As String)
                        myFarm.DSWO = Date.Parse(value)
                End Set
        End Property

        'Percentage wintered off
        <Output()> <Units("%")> Public ReadOnly Property PercentageWinteredOff() As Double
                Get
                        Return (1 - (myFarm.StockingRate() / BaseStockingRate)) * 100
                End Get
        End Property

        'Proportion of farm allocated to laneways (not grazed)
        <Output()> <Units("0-1")> Public Property PorportionOfFarmInLaneWays() As Double
                Get
                        Return myFarm.PorportionOfFarmInLaneWays()
                End Get
                Set(ByVal value As Double)
                        myFarm.PorportionOfFarmInLaneWays = value
                End Set
        End Property

        'Hours per day cows are on laneways
        <Output()> <Units("0-24")> Public Property HoursOnLaneWays() As Double
                Get
                        Return myFarm.HoursOnLaneWays()
                End Get
                Set(ByVal value As Double)
                        myFarm.HoursOnLaneWays = value
                End Set
        End Property

        'Hours per day cows are in the dairy shed
        <Output()> <Units("0-24")> Public Property HoursInDairyShed() As Double
                Get
                        Return myFarm.HoursInDairyShed()
                End Get
                Set(ByVal value As Double)
                        myFarm.HoursInDairyShed = value
                End Set
        End Property

        'Amount of effluient being held in storage
        <Output()> <Units("TBC")> Public ReadOnly Property EffluientPondVolume() As Double
                Get
                        Return myFarm.myEffluentPond.Volume
                End Get
        End Property
#End Region

#Region "4b. USe stand-off (assumes that cows still eat pasture)"
#End Region
#End Region

        Private Sub PrintFarmSummary()
                Console.WriteLine()
                Console.WriteLine("---------- DDRules Initialisation ----------")
                Console.WriteLine("     General Farm Description")
                Console.WriteLine("             Effective Area          " & myFarm.FarmArea)
                Console.WriteLine("             Total Paddocks          " & myFarm.PaddockCount)
                Console.WriteLine("     Stock Management")
                Console.WriteLine("             Stocking Rate           " & BaseStockingRate)
                Console.WriteLine("             Calving Date            ")
                Console.WriteLine("             Paddock Count           " & myFarm.PaddockCount)
                Console.WriteLine("             Winter Off Dry Stock    " & myFarm.WinterOffDryStock.ToString)
                Console.WriteLine("     Grazing Management")
                Console.WriteLine("             Residules               " & myFarm.GrazingResidual)
                Console.WriteLine("             Interval                " & myFarm.GrazingInterval)
                Console.WriteLine("     Supplementary Feeding")
                Console.WriteLine("             ME Content (ME/kgDM)    " & myFarm.SupplementME)
                Console.WriteLine("             N Content               " & myFarm.SupplementN * 100 & "%")
                Console.WriteLine("             Wastage                 " & myFarm.SupplementWastage * 100 & "%")
                Console.WriteLine("     Conservation")
                Console.WriteLine("             Start Date              " & myFarm.FCD.ToString("dd-MMM"))
                Console.WriteLine("             Finish Date             " & myFarm.LCD.ToString("dd-MMM"))
                Console.WriteLine("             Trigger Residule        " & myFarm.CDM)
                Console.WriteLine("             Cutting Residule        " & myFarm.CR)
                Console.WriteLine("             Wastage at cutting      " & myFarm.SilageCutWastage & "%")
                Console.WriteLine("             Silage Stored on Farm   " & myFarm.EnableSilageStore.ToString)
                If (myFarm.EnableSilageStore) Then
                        Console.WriteLine("             ME Content (ME/kgDM)    " & myFarm.SilageME)
                        Console.WriteLine("             N Content               " & myFarm.SilageN * 100 & "%")
                        Console.WriteLine("             Wastage (at feeding)    " & myFarm.SilageWastage * 100 & "%")
                End If
                Console.WriteLine("     Debug Switches")
                Console.WriteLine("             Debug Level   " & DebugLevel)
                Console.WriteLine("             Break Feeding " & BreakFeeding)
                Console.WriteLine("             Avg length    " & GrowthRateWindowSize)
        End Sub

        'Detail level of debug output (0=none, 1=some, 2=detailed, 3=very detailed)
        <Param()> Property DebugLevel() As Integer
                Get
                        Return myDebugLevel
                End Get
                Set(ByVal value As Integer)
                        myDebugLevel = value
                        myFarm.DebugLevel = value - 1
                End Set
        End Property

        'Interpolate monthly values to get daily values (warning: this can cause issues) [0=No, 1=Yes]
        <Param()> Property Cow_Interpolation() As Integer
                Get
                        Return SimpleCow.DoInterpolate
                End Get
                Set(ByVal value As Integer)
                        SimpleCow.DoInterpolate = value
                End Set
        End Property

        'Control animal grazing to simulate break feeding of indervidual paddocks [0=No, 1=Yes]
        <Param()> Property BreakFeeding() As Integer
                Get
                        Return LocalPaddockType.DebugTestBreakFeeding
                End Get
                Set(ByVal value As Integer)
                        LocalPaddockType.DebugTestBreakFeeding = value
                End Set
        End Property

        'Testing: Use a moving average of daily growth to predict future growth
        <Param()> Property GrowthRateWindowSize() As Integer
                Get
                        Return LocalPaddockType.MovingAverageSeriesLength
                End Get
                Set(ByVal value As Integer)
                        LocalPaddockType.MovingAverageSeriesLength = value
                End Set

        End Property

        'Porportion of farm to return effluient to
        <Output()> <Units("0-1")> Public Property EffluentPaddocksPercentage() As Double
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


        'Average pasture mass across the milking platform
        <Output()> <Units("kg/ha")> Public ReadOnly Property AverageFarmCover() As Double
                Get
                        Return myFarm.AverageCover
                End Get
        End Property

        'Amount of dry matter on farm in relation to the amount required to meet animal demands (+ve = surplus, -ve deficit)
        <Output()> <Units("kg/ha")> Public ReadOnly Property FeedSituation() As Double
                Get
                        Return myFarm.FeedSituation()
                End Get
        End Property

        'Ideal average pasture mass across the milking platform in order to meet animal demands
        <Output()> <Units("kg/ha")> Public ReadOnly Property IdealAvgGrazingCover() As Double
                Get
                        Return myFarm.IdealGrazingCover()
                End Get
        End Property

        'Ideal pre-grazing pasture mass required to meet animal demands, post-grazing residual and rotation length
        <Output()> <Units("kg/ha")> Public ReadOnly Property IdealPreGrazingCover() As Double
                Get
                        Return myFarm.IdealPreGrazingCover()
                End Get
        End Property

        Dim myIrrigation_efficiency As Double

        'This can be set but is not used yet
        <Output()> <Units("0-1")> Public Property irrigation_efficiency() As Double
                Get
                        Return myIrrigation_efficiency
                End Get
                Set(ByVal value As Double)
                        If (value < 0) Then
                                myIrrigation_efficiency = 0
                        ElseIf (value > 1) Then
                                myIrrigation_efficiency = 1
                        Else
                                myIrrigation_efficiency = value
                        End If
                End Set
        End Property

        'Test array output (not used)
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

        'Average plant available water (proportion availble in the top 300mm)
        <Output()> <Units("0-1")> Public ReadOnly Property PlantAvalibleWater As Single
                Get
                        Return myFarm.PlantAvalibleWater(300)
                End Get
        End Property

        'Set milk production data
        <Output()> <Units("kgMS/cow/day")> Public Property MilkCurve As String
                Get
                        Return "MilkCurve"
                End Get
                Set(ByVal value As String)
                        Dim strValues As String() = value.Split(",")
                        Dim values(11) As Double
                        For i As Integer = 0 To strValues.Length - 1
                                If (DebugLevel > 0) Then
                                        Console.Write("Milk Curve = " + strValues(i))
                                End If
                                values(i) = strValues((i + 6) Mod 12)
                                If (DebugLevel > 0) Then
                                        Console.WriteLine(" [ha] = " + (values(i) * BaseStockingRate).ToString("0.00"))
                                End If
                        Next
                        myFarm.setMilkSolids(values)
                End Set
        End Property

        'Set paddocks to return effluent to
        <Output()> <Units("")> Public Property EffluentPaddocks As String
                Get
                        Dim result As String = ""
                        For Each str As String In myFarm.getEffluentPaddocks()
                                result += str + ","
                        Next
                        'result = result.Substring(0, result.Length - 2)
                        If (result.Length > 0) Then
                                Return result.Substring(0, result.Length - 1)
                        End If
                        Return result
                End Get
                Set(ByVal value As String)
                        Dim strValues As String() = value.Split(",")
                        Dim values(strValues.Length - 1) As String
                        For i As Integer = 0 To strValues.Length - 1
                                If (DebugLevel > 0) Then
                                        Console.Write("EffluentPaddocks = " + strValues(i))
                                End If
                                values(i) = strValues(i).TrimEnd
                                If (DebugLevel > 0) Then
                                        Console.WriteLine(" = " + values(i))
                                End If
                        Next
                        myFarm.setEffluentPaddocks(values)
                End Set
        End Property

        'Set Live Weight Profile
        <Output()> <Units("kgMS/cow/day")> Public Property LWtCurve As String
                Get
                        Return "LWtCurve"
                End Get
                Set(ByVal value As String)
                        Dim strValues As String() = value.Split(",")
                        Dim values(12) As Double
                        For i As Integer = 0 To strValues.Length - 1
                                If (DebugLevel > 0) Then
                                        Console.Write("LWt Curve = " + strValues(i))
                                End If
                                values(i) = strValues((i + 6) Mod 12)
                                If (DebugLevel > 0) Then
                                        Console.WriteLine(" [kg/ha] = " + (values(i) * BaseStockingRate).ToString("0.00"))
                                End If
                        Next
                        myFarm.setLiveWeight(values)
                End Set
        End Property

        'Grazing allocation system used (0=Dawns rules, 1=LUDF)
        <Output()> <Units("[0,1]")> Public Property AllocationType() As Integer
                Get
                        Return myFarm.AllocationType
                End Get
                Set(ByVal value As Integer)
                        myFarm.AllocationType = value
                End Set
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