Imports System
Imports System.Collections.Generic
Imports System.Text
Imports ModelFramework
Imports System.Xml
Imports System.Xml.Schema
Imports CSGeneral
Imports ManagerHelpers

'Bugs:
'    Rotation lenghts not being respected. Need to implement the day count type method or fix paddock status

Public Class DDRules
        Inherits Instance
        Dim debug As Boolean = False
        Private myFarm As Farm
        Private myHerd As SimpleHerd 'local handle to the herd contained in Farm. This is only a short term fix
        Private MyPaddock As PaddockType

        <Input()> Private month As Integer
        <Input()> Private year As Integer
        <Input()> Private day_of_month As Integer
        <Input()> Private end_week As Integer
        '<Input()> Private UI_StockRate As Single
        '<Input()> Private UI_SuppType As String

        'Run with FarmSim default values
        <Param()> <Output()> Public EnableFarmSim As Integer
        'Stocking rate a peak of lactation
        <Output()> <Units("cows/ha")> Public BaseStockingRate As Double = 2.5 'default value

        'Dawn's pasture ME / month table. Will be replace with calls to ApSim
        <Output()> <Units("cows/ha")> Public Property InitialStockingRate() As Single
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

        'Current stocking rate on farm (i.e. excludes cows wintering off)
        <Output()> <Units("cows/ha")> Public Property sr() As Single
                Get
                        Return myFarm.StockingRate
                End Get
                Set(ByVal value As Single)
                        myFarm.StockingRate = value
                End Set
        End Property

        Public dairyNZ_mg As Single() = {20, 25, 30, 40, 50, 100, 100, 80, 50, 25, 20, 20}  'jan to dec
        'Public dairyNZ_gr As Single() = {1600, 1600, 1600, 1500, 1400, 1200, 1200, 1400, 1500, 1500, 1500, 1500}  'june to may
        Public dairyNZ_gr As Single() = {1600, 1600, 1600, 1600, 1600, 1200, 1200, 1600, 1600, 1600, 1600, 1600}  'june to may
        ' tdf < 0 -> replaced by SimpleCow
        Public tfd As Single = -1   '{80, 100, 140, 180, 200, 200, 190, 180, 170, 160, 150, 140}  'june to may
        'Total feed demand
        <Output()> <Units("MJME")> Public localTFD As Double = 0
        'Farm Pasture area
        <Output()> <Units("ha")> Public localFPA As Double = 0
        'Grazing residual
        <Output()> <Units("kgDM")> Public localGR As Integer = 0
        'Minimum grazing interval
        <Output()> <Units("days")> Public localMG As Double = 0
        'Maximum area available
        <Output()> <Units("ha")> Public localMA As Double = 0
        'Pasture ME content
        <Output()> <Units("MJME")> Public localPME As Double
        'Pasture harvested
        <Output()> <Units("kgDM")> Public PastureHarvested As Double = 0.0
        'ME still required to meet animal requirements
        <Output()> <Units("MJME")> Public ReadOnly Property RemainingFeedDemand() As Double
                Get
                        Return myHerd.RemainingFeedDemand
                End Get
        End Property

        'Total animal requirements [MJME] (for the full herd)
        <Output()> <Units("MJME")> Public ReadOnly Property TotalFeedDemand() As Double
                Get
                        Return myHerd.TodaysEnergyRequirement
                End Get
        End Property

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

        Private FirstSimulationDay = True

        Public Sub New()
                myFarm = New Farm()
        End Sub
#Region "EventHandlers"
        <EventHandler()> Public Sub OnInit2()
                If (debug) Then
                        Console.WriteLine("Enter OnInit2()")
                End If

                ' ************* Farm testing **********************
                MyPaddock = New PaddockType(Me)
                'myFarm = New Farm() 'moved to constructor
                myFarm.Init(MyPaddock, year, month)

                myHerd = myFarm.getHerd()
                ' ************* Farm testing **********************
                SetupFarmSim()
                'If (debug) Then
                PrintFarmSummary()
                'End If
        End Sub

        Sub SetupFarmSim()
                Dim FarmSim As ComponentType = MyPaddock.Component("FarmSimGraze")
                If FarmSim Is Nothing Then
                        Return
                End If

                Dim UI_StockRate As Double = FarmSim.Variable("UI_StockRate").ToDouble
                Dim UI_SuppType As String = FarmSim.Variable("UI_SuppType").ToDouble

                BaseStockingRate = UI_StockRate
                sr = BaseStockingRate

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
                myFarm.WinterOffDryStock = True 'all stock wintered off farm
        End Sub

        <EventHandler()> Sub OnPrepare()
                If (debug) Then
                        Console.WriteLine("Enter OnPrepare()")
                End If

                GrazingInterval = dairyNZ_mg(month - 1) 'Minium Grazing Interval (actually rotation length)
                GrazingResidual = dairyNZ_gr(month - 1) 'Grazing residual

                ' ************* Farm testing **********************
                myFarm.GrazingResidual = GrazingResidual
                myFarm.GrazingInterval = GrazingInterval
                myFarm.Prepare(year, month, day_of_month, end_week)
                myFarm.StockingRate = BaseStockingRate
                ' ************* Farm testing **********************

                If (debug) Then
                        Console.WriteLine("   Rotation Length " & GrazingInterval.ToString)
                        Console.WriteLine("   Residual " & GrazingResidual.ToString)
                        Console.WriteLine("   SR " & sr.ToString)
                End If

        End Sub

        <EventHandler()> Sub OnProcess()
                If (day_of_month = 1 And month = 1) Then
                        Console.WriteLine("")
                End If

                ' ************* Farm testing **********************
                myFarm.GrazingResidual = GrazingResidual
                myFarm.GrazingInterval = GrazingInterval
                'myFarm.StockingRate = sr
                myFarm.Process()
                Dim cover = myFarm.AverageCover
                ' ************* Farm testing **********************

                FirstSimulationDay = False
                PrepareOutputs()
        End Sub

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
#End Region

#Region "CowProperties"

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
        <Output()> <Units("kgMS")> Public ReadOnly Property MilkProduction() As Double
                Get
                        Return myHerd.MS_per_Day
                End Get
        End Property

        'Month of lactation
        <Output()> <Units("kgMS")> Public ReadOnly Property MLact() As Double
                Get
                        Return myHerd.Month_Of_Lactation
                End Get
        End Property

        'Month of pregnancy
        <Output()> <Units("kgMS")> Public ReadOnly Property MPreg() As Double
                Get
                        Return myHerd.Month_Of_Pregnancy
                End Get
        End Property

        'Average cow live weight
        <Output()> <Units("kg_LWt")> Public ReadOnly Property Cow_LWt() As Double
                Get
                        Return myHerd.Live_Weight
                End Get
        End Property

        'Average cows change in live weight
        <Output()> <Units("kg_LWt")> Public ReadOnly Property Cow_dLWt() As Double
                Get
                        Return myHerd.LWt_Change
                End Get

        End Property

        'Averge body condition score
        <Output()> <Units("kg_LWt")> Public ReadOnly Property Cow_BC() As Double
                Get
                        Return myHerd.BC
                End Get
        End Property

        'Milk solids production per cow
        <Output()> <Units("kg_LWt")> Public ReadOnly Property Cow_MilkSolids() As Double
                Get
                        Return myHerd.MS_per_Day_Cow
                End Get
        End Property

        'Is herd currently dried off
        <Output()> <Units("")> Public ReadOnly Property Cow_IsDry() As String
                Get
                        Return myHerd.isDry.ToString
                End Get
        End Property
#End Region

        'Current grazing interval / return period
        <Output()> <Units("Days")> Public Property GrazingInterval() As Integer
                Get
                        Return myFarm.GrazingInterval
                End Get
                Set(ByVal value As Integer)
                        myFarm.GrazingInterval = value
                End Set
        End Property

        'Current cow grazing residual
        <Output()> <Units("kgDM/ha")> Public Property GrazingResidual() As Integer
                Get
                        Return myFarm.GrazingResidual
                End Get
                Set(ByVal value As Integer)
                        'localGR = value
                        'updateCovers(localGR, localPME)
                        myFarm.GrazingResidual = value
                End Set
        End Property

        'Quantity of silage cut on farm today
        <Output()> <Units("kgDM")> Public ReadOnly Property SilageCut() As Double
                Get
                         Return myFarm.SilageCut
                End Get
        End Property

        'Quantity of silage cut on farm today
        <Output()> <Units("kgDM/ha")> Public ReadOnly Property SilageCut_kgha() As Double
                Get
                        Return myFarm.SilageCut / myFarm.FarmArea
                End Get
        End Property

#Region "2: Feeding Supplements"
        'Energy content of purchased supplement
        <Output()> <Units("MJME")> Public Property SupplementME() As Double
                Get
                        Return myFarm.SupplementME
                End Get
                Set(ByVal value As Double)
                        myFarm.SupplementME = value
                End Set
        End Property

        'Nitrogen content of purchased supplement
        <Output()> <Units("MJME")> Public Property SupplementN() As Double
                Get
                        Return myFarm.SupplementN
                End Get
                Set(ByVal value As Double)
                        myFarm.SupplementN = value
                End Set
        End Property

        'Supplment loss at feeding out
        <Output()> <Units("%")> Public Property SupplementWastage() As Double
                Get
                        Return myFarm.SupplementWastage
                End Get
                Set(ByVal value As Double)
                        myFarm.SupplementWastage = value
                End Set
        End Property
#End Region

#Region "3: Conservation"
        Private mySilageStoreEnable As Integer = 0
        Private mySilageStore As Integer = 0
        ' Conservation trigger pasture mass (Dawn default = 3500)
        <Output()> <Units("kgDM/ha")> Public CDM As Double = 3500
        ' Conservation cutting residual pasture mass (Dawn default = 1600)
        <Output()> <Units("kgDM/ha")> Public CR As Integer = 1600
        '<Output()> <Units("MJME")> Public SilageME As Double = 10.5 ' ME content of the silage
        'First Conservation Date - uing a month for the time being
        <Output()> <Units("MJME")> Public FCD As Integer = 9
        'Last Conservation Date - uing a month for the time being
        <Output()> <Units("MJME")> Public LCD As Integer = 3

        'Energy content of silage produced on farm
        <Output()> <Units("MJME")> Public Property SilageME() As Double
                Get
                        Return myFarm.SilageME
                End Get
                Set(ByVal value As Double)
                        myFarm.SilageME = value
                End Set
        End Property

        'Nitrogen content of silage produced on farm
        <Output()> <Units("MJME")> Public Property SilageN() As Double
                Get
                        Return myFarm.SilageN
                End Get
                Set(ByVal value As Double)
                        myFarm.SilageN = value
                End Set
        End Property

        'Silage lost during feeding out
        <Output()> <Units("%")> Public Property SilageWastage() As Double
                Get
                        Return myFarm.SilageWastage
                End Get
                Set(ByVal value As Double)
                        myFarm.SilageWastage = value
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

        'Amount of silage avalible for feeding out
        <Output()> <Units("kgDM/ha")> Public Property SilageStore() As Double
                Get
                        Return myFarm.SilageStore
                End Get
                Set(ByVal value As Double)
                        myFarm.SilageStore = value
                End Set
        End Property

        'Conservation trigger mass
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
                DM_Eaten_Supplement = myFarm.DM_Eaten_Supplement
                ME_Demand = myFarm.ME_Demand
                ME_Eaten = myFarm.ME_Eaten
                ME_Eaten_Pasture = myFarm.ME_Eaten_Pasture
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

        'Number of cows on farm
        <Output()> Public ReadOnly Property Total_Cows() As Single
                Get
                        Return myHerd.Number_Of_Cows
                End Get
        End Property

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
        'Energy consumed as pasture
        <Output()> <Units("MJME/ha")> Public ME_Eaten_Pasture As Single
        'Energy consumed as supplement
        <Output()> <Units("MJME/ha")> Public ME_Eaten_Supplement As Single
        'Total dry matter consumed
        <Output()> <Units("kgDM/ha")> Public DM_Eaten As Single
        'Dry matter consumed as pasture
        <Output()> <Units("kgDM/ha")> Public DM_Eaten_Pasture As Single
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
        <Output()> <Units("kgN/ha")> Public DM_to_feaces As Single

        'Current status of paddocks
        <Output()> <Units("MJME/ha")> Public ReadOnly Property PaddockStatus() As String()
                Get
                        Return myFarm.PaddockStatus
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
        'Dry matter consumed per paddock
        <Output()> <Units("MJME/ha")> Public ReadOnly Property DM_Eaten_Pdks() As Single()
                Get
                        Return myFarm.DM_Eaten_Pdks
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
        <Output()> <Units("kgN/cow")> Public ReadOnly Property LWt_Change_Cow() As Single
                Get
                        Return myHerd.LWt_Change
                End Get
        End Property
#End Region

#Region "Dawn's outputs/parameters"
        'Percentage wintered off
        <Output()> <Units("%")> Public ReadOnly Property PWO() As Double
                Get
                        Return 1 - (myFarm.StockingRate() / BaseStockingRate)
                End Get
        End Property
#End Region

        Private Sub PrintFarmSummary()
                Console.WriteLine()
                Console.WriteLine("---------- DDRules Initialisation ----------")
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
                Console.WriteLine("             Silage Stored on Farm   " & myFarm.EnableSilageStore.ToString)
                If (myFarm.EnableSilageStore) Then
                        Console.WriteLine("             ME Content (ME/kgDM)    " & myFarm.SilageME)
                        Console.WriteLine("             N Content               " & myFarm.SilageN * 100 & "%")
                        Console.WriteLine("             Wastage                 " & myFarm.SilageWastage * 100 & "%")
                End If
                Console.WriteLine("     Debug Switches")
                Console.WriteLine("             Outputs Apsim Event calls " & debug)
                Console.WriteLine("             Paddock Messaging Level       " & myFarm.DebugLevel)
        End Sub
End Class