Public Class Farm
        Inherits Instance

        Private debug As Boolean = False
        Private myPaddocks As List(Of LocalPaddockType)         ' Full list of apsim paddocks
        Private myHerd As SimpleHerd                            ' Dairy herd

        Dim PaddockQueue As Queue(Of LocalPaddockType)
        Private GrazedList As List(Of LocalPaddockType)         ' List of grazed paddocks
        Private myGrazingResidual, myGrazingInterval As Integer
        Private myPaddockCounter, myDayPerPaddock As Integer
        Public WinterOffDryStock As Boolean = True

        Private myDate As Date
        Private Day As Integer
        Private Month As Integer
        Private end_week As Integer

        Public Sub New()
                myHerd = New SimpleHerd(1, 6, 0, 6)
        End Sub

        Public Sub Init(ByVal MasterPM As PaddockType, ByVal Year As Integer, ByVal Month As Integer)
                myPaddocks = New List(Of LocalPaddockType)
                PaddockQueue = New Queue(Of LocalPaddockType)
                GrazedList = New List(Of LocalPaddockType)
                Dim i As Integer = -1
                For Each SubPaddock As PaddockType In MasterPM.SubPaddocks
                        i += 1
                        myPaddocks.Add(New LocalPaddockType(i, SubPaddock))
                        If (debug) Then
                                Console.WriteLine("   Paddock " & myPaddocks(i).ToString)
                        End If
                Next
                'myHerd = New SimpleHerd(1, 6, Year, Month) ' moved to constructor
        End Sub

        Public Sub Prepare(ByVal Year As Integer, ByVal Month As Integer, ByVal Day As Integer, ByVal end_week As Integer)
                Me.Day = Day
                Me.Month = Month
                myDate = New Date(Year, Month, Day)
                Me.end_week = end_week
                For Each Paddock As LocalPaddockType In myPaddocks
                        Paddock.OnPrepare()
                Next

                myHerd.onPrepare(1, Year, Month)
        End Sub

        Public Sub Process()
                updateCovers()
                If Not (IsWinteringOff()) Then 'assume all stock wintering off farm i.e. no grazing
                        If (myPaddockCounter <= 0 Or PaddockQueue.Count = 0) Then 'either it is time to shift or have completed a full rotation
                                Allocate_Paddocks()
                        End If
                        Graze()
                        doAnimalsPost()
                        myPaddockCounter -= 1
                End If
                doConservation()
        End Sub

        Sub Graze()
                GrazedList.Clear()
                Dim PastureHarvested As Double = 0
                Dim d As Double = myHerd.RemainingFeedDemand
                Dim c As Double = PaddockQueue.Count

                While (myHerd.RemainingFeedDemand > 1 And PaddockQueue.Count > 0)
                        Dim p As LocalPaddockType = PaddockQueue.Peek()
                        Dim removed As BioMass = myHerd.Graze(p, GrazingResidual)
                        PastureHarvested += removed.DM_Total
                        GrazedList.Add(p)
                        'Console.WriteLine("Grazing " & p.ApSim_ID & " DM = " & p.Cover.ToString)
                        If (myPaddockCounter <= 0) Then 'p.AvalibleDryMater <= 1 Or 
                                PaddockQueue.Dequeue() ' not enough drymatter avaible to bother comming back to or end of rotation
                                ResetPaddockCounter()
                        Else
                                Return
                        End If
                End While
        End Sub

        Private Sub doAnimalsPost()
                If (myHerd.isUnderFed) Then
                        FeedSupplements()
                End If

                myHerd.doNitrogenPartioning()

                If (GrazedList.Count > 0) Then
                        myHerd.doNutrientReturns(GrazedList)
                ElseIf (myPaddocks.Count > 0) Then 'no paddocks grazed today, return nutrients to those paddock allocated as part of the rotation
                        myHerd.doNutrientReturns(myPaddocks)
                End If
        End Sub

#Region "2: Feeding Supplements"
        'Supplementary feeding
        <Output()> <Units("kgDM")> Public SilageFed As Double 'kgDM @ 10.5me fed to meet animal requirements
        <Output()> <Units("kgDM")> Public SupplementFedout As Double 'kg of grain fed this period (to fill unsatisifed feed demand)
        <Param()> <Output()> <Units("%")> Public SupplementWastage As Double = 0.0 'percentage of feed wasted as part of feeding out [Dawns' default = 10%]
        <Param()> <Output()> <Units("%")> Public SupplementDigestability As Double = 0.8
        <Param()> <Output()> <Units("%")> Public SilageWastage As Double = 0.0 'percentage of feed wasted as part of feeding out [Dawns' default = 10%]
        <Param()> <Output()> <Units("%")> Public SilageDigestability As Double = 0.7 'need to check this value

        <Param()> <Units("kgN/kgDM")> Public SNC1 As Double = 0.035 'N content of silage (need to chechk this value - add to the user interface
        <Param()> <Units("kgN/kgDM")> Public SNC2 As Double = 0.018 'N content of supplement (grain?) - add to the user interface
        <Param()> <Units("ME/kgDM")> Private SME As Single = 12

        <Output()> <Units("MJME")> Public Property SupplementME() As Double
                Get
                        Return SME
                End Get
                Set(ByVal value As Double)
                        SME = value
                End Set
        End Property

        <Output()> <Units("MJME")> Public Property SupplementN() As Double
                Get
                        Return SNC2
                End Get
                Set(ByVal value As Double)
                        SNC2 = value
                End Set
        End Property

        <Output()> <Units("MJME")> Public Property SilageN() As Double
                Get
                        Return SNC1
                End Get
                Set(ByVal value As Double)
                        SNC1 = value
                End Set
        End Property

        'Silgae and Supplements will be used to completely fill the remaining demand
        'TODO - check implementation of wastage
        Sub FeedSupplements()
                If (myHerd.RemainingFeedDemand > 0) Then ' Meet any remaining demand with bought in feed (i.e. grain)
                        FeedSilage(myHerd.RemainingFeedDemand, SilageME, SNC1, SilageWastage, SilageDigestability)
                End If

                If (myHerd.RemainingFeedDemand > 0) Then ' Meet any remaining demand with bought in feed (i.e. grain)
                        FeedSupplement(myHerd.RemainingFeedDemand, SupplementME, SNC2, SupplementWastage, SupplementDigestability)
                End If
        End Sub

        Private Property SilageStoreTotal() As Double
                Get
                        Return SilageStore * FarmArea()
                End Get
                Set(ByVal value As Double)
                        SilageStore = value / FarmArea()
                End Set
        End Property
        Function FeedSilage(ByVal MEDemand As Single, ByVal MEperKg As Single, ByVal NperKg As Single, ByVal WastageFactor As Single, ByVal Digestability As Single) As Boolean
                Dim dm As BioMass = New BioMass()
                SilageFed = (MEDemand / MEperKg) * (1 + WastageFactor)
                If (SilageFed > SilageStoreTotal) Then
                        SilageFed = SilageStoreTotal
                End If
                SilageStoreTotal -= SilageFed ' remove silage from the heap

                dm.gLeaf = SilageFed * (1 - WastageFactor)
                dm.setME(SilageME)
                dm.digestibility = Digestability
                dm.N_Conc = NperKg
                myHerd.Feed(dm, False)

                Return (dm.DM_Total > 0)
        End Function

        Function FeedSupplement(ByVal MEDemand As Single, ByVal MEperKg As Single, ByVal NperKg As Single, ByVal WastageFactor As Single, ByVal Digestability As Single) As Boolean
                Dim dm As BioMass = New BioMass()
                SupplementFedout = (MEDemand / MEperKg) * (1 + WastageFactor)
                dm.gLeaf = SupplementFedout * (1 - WastageFactor)
                dm.setME(SupplementME)
                dm.digestibility = Digestability
                dm.N_Conc = NperKg
                myHerd.Feed(dm, False)
                Return (dm.DM_Total > 0)
        End Function
#End Region

        Private Function IsWinteringOff() As Boolean
                Return WinterOffDryStock And myHerd.isDry
        End Function

        Public Property StockingRate() As Double
                Get
                        If (IsWinteringOff()) Then
                                Return 0
                        Else
                                Return myHerd.Number_Of_Cows / FarmArea
                        End If
                End Get
                Set(ByVal value As Double)
                        If (value < 0) Then
                                value = 0
                        End If
                        myHerd.setCowNumbers(value * FarmArea) 'assume 1ha paddocks
                End Set
        End Property
 
        Sub Allocate_Paddocks()
                SortPaddocksByCover()
                PaddockQueue = New Queue(Of LocalPaddockType)
                For Each Paddock As LocalPaddockType In myPaddocks
                        If Not (Paddock.Closed) Then
                                PaddockQueue.Enqueue(Paddock) 'add all paddock to the queue (including close ones)
                        End If
                Next
                ResetPaddockCounter()
        End Sub

        Sub updateCovers()
                For Each Paddock As LocalPaddockType In myPaddocks
                        Paddock.UpdateCovers()
                Next
        End Sub

        Sub updateGrazingResidual(ByVal residual As Integer)
                For Each Paddock As LocalPaddockType In myPaddocks
                        Paddock.GrazingResidual = residual
                Next
        End Sub

        Public Property GrazingResidual() As Integer
                Get
                        Return myGrazingResidual
                End Get
                Set(ByVal value As Integer)
                        If (value <> GrazingResidual) Then
                                If (value > 0) Then
                                        myGrazingResidual = value
                                Else
                                        myGrazingResidual = 0
                                End If
                                updateGrazingResidual(myGrazingResidual)
                        End If

                End Set
        End Property

        Public Property GrazingInterval() As Integer
                Get
                        Return myGrazingInterval
                End Get
                Set(ByVal value As Integer)
                        If (value <> myGrazingInterval) Then
                                If (value >= 0) Then
                                        myGrazingInterval = value
                                Else
                                        myGrazingInterval = 1 'default to set stocking
                                End If
                                myDayPerPaddock = myGrazingInterval / FarmArea
                                Allocate_Paddocks()
                                ResetPaddockCounter()
                        End If
                End Set
        End Property

        Sub ResetPaddockCounter()
                myPaddockCounter = Math.Round(myDayPerPaddock)
        End Sub

        Sub SortPaddocksByCover()
                'shufflePaddocks()
                myPaddocks.Sort(LocalPaddockType.getSortListByCover())
        End Sub

        Sub SortByIndex()
                myPaddocks.Sort(LocalPaddockType.getSortListByIndex())
        End Sub

        Private Sub PrintPaddocks()
                If (debug) Then
                        For Each pdk As LocalPaddockType In myPaddocks
                                Console.WriteLine(pdk.ToString)
                        Next
                End If
        End Sub

        Public Function AverageCover() As Double
                Dim TotalCover As Double = 0
                For Each Paddock As LocalPaddockType In myPaddocks
                        TotalCover += Paddock.Cover
                Next
                Return TotalCover / FarmArea
        End Function

        Public Function FarmArea() As Double
                Return myPaddocks.Count ' assume 1ha paddocks for simplisity
        End Function

        Public Function getHerd() As SimpleHerd
                Return myHerd
        End Function

#Region "3: Pasture Conservation"

        <Input()> <Units("kgDM/ha")> Public CDM As Double = 3500 ' Conservation trigger pasture mass (Dawn default = 3500)
        <Input()> <Units("kgDM/ha")> Public CR As Integer = 1600 ' Conservation cutting residual pasture mass (Dawn default)
        <Input()> <Units("MJME")> Public SilageME As Double = 10.5 ' ME content of the silage
        <Output()> <Units("kgDM")> Public SilageCut As Double
        Public SilageStore As Double 'kgDM/ha fed/silage on hand
        Public PaddocksClosed As Integer = 0 'number of paddocks currently close for conservation
        'should these be moved out to a management script?
        'Public FCD As Integer = 9 'First Conservation Date - uing a month for the time being
        'Public LCD As Integer = 3 'Last Conservation Date - uing a month for the time being
        Public FCD As Date 'New - First Conservation Date
        Public LCD As Date 'New - Last Conservation Date
        Public EnableSilageStore As Boolean = True 'switch ot turn off local storage of farm made silage

        'Private Sub doConservation() 'old month only implmentation
        '        If isBetween(Month, FCD, LCD) Then
        '                ClosePaddocks()
        '        End If

        '        Dim IsCuttingDay As Boolean = end_week > 0 'only cut once a week [as per Dawn's rules]
        '        SilageCut = 0
        '        If (IsCuttingDay And PaddocksClosed) Then
        '                SilageCut = doHarvest()
        '                If (EnableSilageStore) Then
        '                        SilageStore += SilageCut
        '                End If
        '        End If
        'End Sub

        Private Sub doConservation()
                If isBetween(myDate, FCD, LCD) Then
                        ClosePaddocks()
                End If

                Dim IsCuttingDay As Boolean = end_week > 0 'only cut once a week [as per Dawn's rules]
                SilageCut = 0
                If (IsCuttingDay And PaddocksClosed) Then
                        SilageCut = doHarvest()
                        If (EnableSilageStore) Then
                                SilageStoreTotal += SilageCut
                        End If
                End If
        End Sub

        Private Sub ClosePaddocks()
                updateCovers()
                For Each Paddock As LocalPaddockType In myPaddocks
                        If Not (Paddock.Closed) And (Paddock.Cover > CDM) And Not (GrazedList.Contains(Paddock)) Then
                                Paddock.Closed = True
                                PaddocksClosed += 1
                        End If
                Next
        End Sub

        Private Function doHarvest() As Single
                Dim result As Single = 0
                For Each Paddock As LocalPaddockType In myPaddocks
                        If (Paddock.Closed) Then                        'Harvest all closed paddocks
                                result += Paddock.Harvest(CR)
                                PaddockQueue.Enqueue(Paddock)           'add paddock back into the rotation
                        End If
                Next
                PaddocksClosed = 0
                Return result
        End Function
#End Region

#Region "Additional Output Variables"
        Public Sub PrepareOutputs()
                myPaddocks.Sort(LocalPaddockType.getSortListByIndex)
                DM_Eaten = myHerd.DM_Eaten / FarmArea()
                DM_Eaten_Pasture = myHerd.DM_Eaten_Pasture / FarmArea()
                DM_Eaten_Supplement = myHerd.DM_Eaten_Supplement / FarmArea()
                ME_Demand = myHerd.ME_Demand / FarmArea()
                ME_Eaten = myHerd.ME_Eaten / FarmArea()
                ME_Eaten_Pasture = myHerd.ME_Eaten_Pasture / FarmArea()
                ME_Eaten_Supplement = myHerd.ME_Eaten_Supplement / FarmArea()
                N_Eaten = myHerd.N_Eaten / FarmArea()
                N_Eaten_Pasture = myHerd.N_Eaten_Pasture / FarmArea()
                N_Eaten_Supplement = myHerd.N_Eaten_Supplement / FarmArea()
                N_to_milk = myHerd.N_to_Milk / FarmArea()
                N_to_BC = myHerd.N_to_BC / FarmArea()
                N_to_feaces = myHerd.N_to_feaces / FarmArea()
                DM_to_feaces = myHerd.DM_to_feaces / FarmArea()
                N_to_urine = myHerd.N_to_urine / FarmArea()
                N_Balance = myHerd.N_Balance / FarmArea()
                N_Out = myHerd.N_Out / FarmArea()

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

        '<Output()> <Units("()")> Public PaddockStatus As Single
        <Output()> <Units("MJME/ha")> Public ReadOnly Property PaddockStatus() As String()
                Get
                        Dim result(myPaddocks.Count - 1) As String
                        'sort by index
                        For i As Integer = 0 To (myPaddocks.Count - 1)
                                result(i) = myPaddocks(i).StatusCode
                        Next
                        Return result
                End Get
        End Property

        ' by paddock variables
        <Output()> <Units("MJME/ha")> Public ReadOnly Property DM_Eaten_Pdks() As Single()
                Get
                        Dim result(myPaddocks.Count - 1) As Single
                        'sort by index here
                        For i As Integer = 0 To (myPaddocks.Count - 1)
                                result(i) = myPaddocks(i).DM_Eaten()
                        Next
                        Return result
                End Get
        End Property
        <Output()> <Units("MJME/ha")> Public ReadOnly Property ME_Eaten_Pasture_Pdks() As Single()
                Get
                        Dim result(myPaddocks.Count - 1) As Single
                        'sort by index here
                        For i As Integer = 0 To (myPaddocks.Count - 1)
                                result(i) = myPaddocks(i).ME_Eaten()
                        Next
                        Return result
                End Get
        End Property
        <Output()> <Units("MJME/ha")> Public ME_Eaten_Supplement_Pdks As Single()
        <Output()> <Units("MJME/ha")> Public ReadOnly Property ME_Eaten_Pdks() As Single()
                Get
                        Dim result(myPaddocks.Count - 1) As Single
                        'sort by index
                        For i As Integer = 0 To (myPaddocks.Count - 1)
                                result(i) = myPaddocks(i).ME_Eaten()
                        Next
                        Return result
                End Get
        End Property
        <Output()> <Units("kgDM/ha")> Public DM_Eaten_Pasture_Pdks As Single()
        <Output()> <Units("kgDM/ha")> Public DM_Eaten_Supplement_Pdks As Single()
        <Output()> <Units("kgN/ha")> Public ReadOnly Property N_Eaten_Pdks() As Single()
                Get
                        Dim result(myPaddocks.Count - 1) As Single
                        'sort by index
                        For i As Integer = 0 To (myPaddocks.Count - 1)
                                result(i) = myPaddocks(i).N_Eaten()
                        Next
                        Return result
                End Get
        End Property
        <Output()> <Units("kgN/ha")> Public N_Eaten_Pasture_Pdks As Single()
        <Output()> <Units("kgN/ha")> Public N_Eaten_Supplement_Pdks As Single()
        <Output()> <Units("kgN/ha")> Public N_to_milk_Pdks As Single()
        <Output()> <Units("kgN/ha")> Public N_to_BC_Pdks As Single()
        <Output()> <Units("kgN/ha")> Public ReadOnly Property N_to_feaces_Pdks() As Single()
                Get
                        Dim result(myPaddocks.Count - 1) As Single
                        'sort by index
                        For i As Integer = 0 To (myPaddocks.Count - 1)
                                result(i) = myPaddocks(i).N_From_Feaces()
                        Next
                        Return result
                End Get
        End Property
        <Output()> <Units("kgN/ha")> Public ReadOnly Property N_to_urine_Pdks() As Single()
                Get
                        Dim result(myPaddocks.Count - 1) As Single
                        'sort by index
                        For i As Integer = 0 To (myPaddocks.Count - 1)
                                result(i) = myPaddocks(i).N_Eaten()
                        Next
                        Return result
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

        Public Function PaddockCount()
                Return myPaddocks.Count
        End Function
#End Region

        Private Sub shufflePaddocks()
                Dim list() As LocalPaddockType = myPaddocks.ToArray()
                Dim i As Integer = 0
                Dim j As Integer
                Dim tmp As LocalPaddockType

                While i < list.Length
                        j = Rnd(list.Length)
                        tmp = list(i)
                        list(i) = list(j)
                        list(j) = tmp
                        i += 1
                End While

                myPaddocks.Clear()
                myPaddocks.AddRange(list)
        End Sub

        Public Property DebugLevel() As Integer
                Get
                        If (myPaddocks.Count > 0) Then
                                Return myPaddocks(0).DebugLevel
                        Else
                                Return 0
                        End If
                End Get
                Set(ByVal value As Integer)
                        For Each paddock As LocalPaddockType In myPaddocks
                                paddock.DebugLevel = value
                        Next
                End Set
        End Property

End Class
