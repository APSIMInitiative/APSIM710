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

        <Param()> <Output()> Public EnableFarmSim As Integer
        <Output()> Public BaseStockingRate As Double = 0

        'Dawn's pasture ME / month table. Will be replace with calls to ApSim
        <Output()> Public Property sr() As Single
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
        <Output()> <Units("MJME")> Public localTFD As Double = 0 'Total Feed Demand
        <Output()> <Units("ha")> Public localFPA As Double = 0 'Farm Pasture area
        <Output()> <Units("kgDM")> Public localGR As Integer = 0 'Grazing residual
        <Output()> <Units("days")> Public localMG As Double = 0 'Minimum grazing interval
        <Output()> <Units("ha")> Public localMA As Double = 0 'Maximum area available
        <Output()> <Units("MJME")> Public localPME As Double 'Pasture ME content
        <Output()> <Units("kgDM")> Public PastureHarvested As Double = 0.0 ' pasture harvested
        <Output()> <Units("MJME")> Public ReadOnly Property RemainingFeedDemand() As Double
                Get
                        Return myHerd.RemainingFeedDemand 'ME fed to meet animal requirements
                End Get
        End Property
        <Output()> <Units("MJME")> Public ReadOnly Property TotalFeedDemand() As Double
                Get
                        Return myHerd.TodaysEnergyRequirement 'Total animal requirements [MJME] (for the full herd)
                End Get
        End Property
        'this stocking is the acual number of cows on farm i.e. normal stocking rate less cows wintering off
        <Output()> Public ReadOnly Property StockingRate() As Single
                Get
                        Return myFarm.StockingRate
                End Get
        End Property

        Private FirstSimulationDay = True
#Region "EventHandlers"
        <EventHandler()> Public Sub OnInit2()
                If (debug) Then
                        Console.WriteLine("Enter OnInit2()")
                End If

                ' ************* Farm testing **********************
                MyPaddock = New PaddockType(Me)
                myFarm = New Farm()
                myFarm.Init(MyPaddock, year, month)
                myHerd = myFarm.getHerd()
                ' ************* Farm testing **********************

                BaseStockingRate = 2.5

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
                myFarm.Prepare(year, month, end_week)
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
#End Region

#Region "CowProperties"

#Region "ME Requirements (Herd)"
        <Output()> <Units("MJME")> Public ReadOnly Property ME_Demand_Maintance() As Double
                Get
                        Return myHerd.ME_Maintance
                End Get
        End Property
        <Output()> <Units("MJME")> Public ReadOnly Property ME_Demand_Lactation() As Double
                Get
                        Return myHerd.ME_Lactation
                End Get
        End Property
        <Output()> <Units("MJME")> Public ReadOnly Property ME_Demand_Pregnancy() As Double
                Get
                        Return myHerd.ME_Pregnancy
                End Get
        End Property
        <Output()> <Units("MJME")> Public ReadOnly Property ME_Demand_Walking() As Double
                Get
                        Return myHerd.ME_Walking
                End Get
        End Property
        <Output()> <Units("MJME")> Public ReadOnly Property ME_Demand_WeightChange() As Double
                Get
                        Return myHerd.ME_WeightChange
                End Get
        End Property
        <Output()> <Units("MJME")> Public ReadOnly Property ME_Demand_Total() As Double
                Get
                        Return myHerd.ME_Total
                End Get
        End Property
#End Region
#Region "ME Requirements (Cow)"
        <Output()> <Units("MJME")> Public ReadOnly Property ME_Demand_Total_Cow() As Double
                Get
                        Return myHerd.ME_Total_Cow
                End Get
        End Property
        <Output()> <Units("MJME")> Public ReadOnly Property ME_Demand_Maintance_Cow() As Double
                Get
                        Return myHerd.ME_Maintance_Cow
                End Get
        End Property
        <Output()> <Units("MJME")> Public ReadOnly Property ME_Demand_Lactation_Cow() As Double
                Get
                        Return myHerd.ME_Lactation_Cow
                End Get
        End Property
        <Output()> <Units("MJME")> Public ReadOnly Property ME_Demand_WeightChange_Cow() As Double
                Get
                        Return myHerd.ME_WeightChange_Cow
                End Get
        End Property
        <Output()> <Units("MJME")> Public ReadOnly Property ME_Demand_Pregnancy_Cow() As Double
                Get
                        Return myHerd.ME_Pregnancy_Cow
                End Get
        End Property
        <Output()> <Units("MJME")> Public ReadOnly Property ME_Demand_Walking_Cow() As Double
                Get
                        Return myHerd.ME_Walking_Cow
                End Get
        End Property
#End Region
        <Output()> <Units("kgMS")> Public ReadOnly Property MilkProduction() As Double
                Get
                        Return myHerd.MS_per_Day
                End Get
        End Property
        <Output()> <Units("kgMS")> Public ReadOnly Property MLact() As Double
                Get
                        Return myHerd.Month_Of_Lactation
                End Get
        End Property
        <Output()> <Units("kgMS")> Public ReadOnly Property MPreg() As Double
                Get
                        Return myHerd.Month_Of_Pregnancy
                End Get
        End Property
        <Output()> <Units("kg_LWt")> Public ReadOnly Property Cow_LWt() As Double
                Get
                        Return myHerd.Live_Weight
                End Get
        End Property
        <Output()> <Units("kg_LWt")> Public ReadOnly Property Cow_dLWt() As Double
                Get
                        Return myHerd.LWt_Change
                End Get
        End Property
        <Output()> <Units("kg_LWt")> Public ReadOnly Property Cow_BC() As Double
                Get
                        Return myHerd.BC
                End Get
        End Property
        <Output()> <Units("kg_LWt")> Public ReadOnly Property Cow_MilkSolids() As Double
                Get
                        Return myHerd.MS_per_Day_Cow
                End Get
        End Property
        <Output()> <Units("")> Public ReadOnly Property Cow_IsDry() As String
                Get
                        Return myHerd.isDry.ToString
                End Get
        End Property
#End Region

        <Output()> <Units("Days")> Public Property GrazingInterval() As Integer
                Get
                        Return myFarm.GrazingInterval
                End Get
                Set(ByVal value As Integer)
                        myFarm.GrazingInterval = value
                End Set
        End Property

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

        <Output()> <Units("kgDM")> Public ReadOnly Property SilageCut() As Double
                Get
                         Return myFarm.SilageCut
                End Get
        End Property

        <Output()> <Units("kgDM/ha")> Public ReadOnly Property SilageCut_kgha() As Double
                Get
                        Return myFarm.SilageCut / myFarm.FarmArea
                End Get
        End Property

#Region "2: Feeding Supplements"
        <Output()> <Units("MJME")> Public Property SupplementME() As Double
                Get
                        Return myFarm.SupplementME
                End Get
                Set(ByVal value As Double)
                        myFarm.SupplementME = value
                End Set
        End Property
        <Output()> <Units("MJME")> Public Property SupplementN() As Double
                Get
                        Return myFarm.SupplementN
                End Get
                Set(ByVal value As Double)
                        myFarm.SupplementN = value
                End Set
        End Property
        <Output()> <Units("%")> Public Property SupplementWastage() As Double
                Get
                        Return myFarm.SupplementWastage
                End Get
                Set(ByVal value As Double)
                        myFarm.SupplementWastage = value
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
        <Output()> <Units("kgN/ha")> Public N_Balance As Single
        <Output()> <Units("kgN/ha")> Public N_Out As Single

        <Output()> <Units("MJME/ha")> Public ME_Demand As Single
        <Output()> <Units("MJME/ha")> Public ME_Eaten As Single
        <Output()> <Units("MJME/ha")> Public ME_Eaten_Pasture As Single
        <Output()> <Units("MJME/ha")> Public ME_Eaten_Supplement As Single
        <Output()> <Units("kgDM/ha")> Public DM_Eaten As Single
        <Output()> <Units("kgDM/ha")> Public DM_Eaten_Pasture As Single
        <Output()> <Units("kgDM/ha")> Public DM_Eaten_Supplement As Single
        <Output()> <Units("kgN/ha")> Public N_Eaten As Single
        <Output()> <Units("kgN/ha")> Public N_Eaten_Pasture As Single
        <Output()> <Units("kgN/ha")> Public N_Eaten_Supplement As Single
        <Output()> <Units("kgN/ha")> Public N_to_milk As Single
        <Output()> <Units("kgN/ha")> Public N_to_BC As Single
        <Output()> <Units("kgN/ha")> Public N_to_feaces As Single
        <Output()> <Units("kgN/ha")> Public N_to_urine As Single
        <Output()> <Units("kgN/ha")> Public DM_to_feaces As Single ' added

        <Output()> <Units("MJME/ha")> Public ReadOnly Property PaddockStatus() As String()
                Get
                        Return myFarm.PaddockStatus
                End Get
        End Property

        ' by paddock variables
        <Output()> <Units("MJME/ha")> Public ReadOnly Property DM_Eaten_Pdks() As Single()
                Get
                        Return myFarm.DM_Eaten_Pdks
                End Get
        End Property
        <Output()> <Units("MJME/ha")> Public ReadOnly Property ME_Eaten_Pasture_Pdks() As Single()
                Get
                        Return myFarm.ME_Eaten_Pasture_Pdks
                End Get
        End Property
        <Output()> <Units("MJME/ha")> Public ME_Eaten_Supplement_Pdks As Single()
        <Output()> <Units("MJME/ha")> Public ReadOnly Property ME_Eaten_Pdks() As Single()
                Get
                        Return myFarm.ME_Eaten_Pdks
                End Get
        End Property
        <Output()> <Units("kgDM/ha")> Public DM_Eaten_Pasture_Pdks As Single()
        <Output()> <Units("kgDM/ha")> Public DM_Eaten_Supplement_Pdks As Single()
        <Output()> <Units("kgN/ha")> Public ReadOnly Property N_Eaten_Pdks() As Single()
                Get
                        Return myFarm.N_Eaten_Pdks
                End Get
        End Property
        <Output()> <Units("kgN/ha")> Public N_Eaten_Pasture_Pdks As Single()
        <Output()> <Units("kgN/ha")> Public N_Eaten_Supplement_Pdks As Single()
        <Output()> <Units("kgN/ha")> Public N_to_milk_Pdks As Single()
        <Output()> <Units("kgN/ha")> Public N_to_BC_Pdks As Single()
        <Output()> <Units("kgN/ha")> Public ReadOnly Property N_to_feaces_Pdks() As Single()
                Get
                        Return myFarm.N_to_feaces_Pdks
                End Get
        End Property
        <Output()> <Units("kgN/ha")> Public ReadOnly Property N_to_urine_Pdks() As Single()
                Get
                        Return myFarm.N_to_urine_Pdks
                End Get
        End Property
        <Output()> <Units("MJME/cow")> Public ME_Demand_Cow As Single
        <Output()> <Units("MJME/cow")> Public ME_Eaten_Cow As Single
        <Output()> <Units("MJME/cow")> Public ME_Eaten_Pasture_Cow As Single
        <Output()> <Units("MJME/cow")> Public ME_Eaten_Supplement_Cow As Single
        <Output()> <Units("kgDM/cow")> Public DM_Eaten_Cow As Single
        <Output()> <Units("kgDM/cow")> Public DM_Eaten_Pasture_Cow As Single
        <Output()> <Units("kgDM/cow")> Public DM_Eaten_Supplement_Cow As Single
        <Output()> <Units("kgN/cow")> Public N_Eaten_Cow As Single
        <Output()> <Units("kgN/cow")> Public N_Eaten_Pasture_Cow As Single
        <Output()> <Units("kgN/cow")> Public N_Eaten_Supplement_Cow As Single
        <Output()> <Units("kgN/cow")> Public N_to_milk_Cow As Single
        <Output()> <Units("kgN/cow")> Public N_to_BC_Cow As Single
        <Output()> <Units("kgN/cow")> Public N_to_feaces_Cow As Single
        <Output()> <Units("kgN/cow")> Public N_to_urine_Cow As Single
        <Output()> <Units("kgN/cow")> Public ReadOnly Property LWt_Change_Cow() As Single
                Get
                        Return myHerd.LWt_Change
                End Get
        End Property
#End Region

#Region "Dawn's outputs/parameters"
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
                Console.WriteLine("             ME Content              " & myFarm.SupplementME)
                Console.WriteLine("             N Content               " & myFarm.SupplementN)
                Console.WriteLine("             Wastage                 " & myFarm.SupplementWastage)
                Console.WriteLine("     Conservation")
                Console.WriteLine("             Start Date              " & myFarm.FCD)
                Console.WriteLine("             Finish Date             " & myFarm.LCD)
                Console.WriteLine("             Trigger Residule        " & myFarm.CDM)
                Console.WriteLine("             Cutting Residule        " & myFarm.CR)
                Console.WriteLine("             Silage Stored on Farm   " & myFarm.EnableSilageStore.ToString)
        End Sub
End Class