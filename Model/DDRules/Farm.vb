Imports ModelFramework

Public Class Farm
        Inherits Instance
        Private myDebugLevel As Integer = 0
        Private myPaddocks As List(Of LocalPaddockType)         ' Full list of apsim paddocks
        Private myPaddocks2 As Dictionary(Of String, LocalPaddockType)         ' Full list of apsim paddocks
        Private myHerd As SimpleHerd                            ' Dairy herd / on milking platform
        Private MyFarmArea As Double
        Private myLaneways As LocalPaddockType

        Dim PaddockQueue As Queue(Of LocalPaddockType)
        Private GrazedList As List(Of LocalPaddockType)         ' List of grazed paddocks
        Private myGrazingResidual As Integer = 1600
        Private myGrazingInterval As Integer = 30
        Private myDayPerHa As Double 'this value is used to control rotation speed
        Public WinterOffDryStock As Boolean = True

        Private myDate As Date
        Private Day As Integer
        Private Month As Integer
        Private end_week As Integer
        Private myPorportionOfFarmInLaneWays As Double = 0
        Private myTimeOnLaneWays As Double = 0
        Private myTimeInDairyShed As Double = 0
        Public myEffluentPond As New EffluentPond
        Public myEffluentIrrigator As New EffluentIrrigator
        Public myEffluentPaddocksPercentage As Double = 1.0 '[default] spread to all paddock
        Public AllocationType As Integer = 0

        Public Sub New()
                myHerd = New SimpleHerd()
        End Sub

        Public Sub Init(ByVal MasterPM As Paddock, ByVal Year As Integer, ByVal Month As Integer, ByVal FarmArea As Double)
                myPaddocks = New List(Of LocalPaddockType)
                myPaddocks2 = New Dictionary(Of String, LocalPaddockType)
                PaddockQueue = New Queue(Of LocalPaddockType)
                GrazedList = New List(Of LocalPaddockType)
                Dim i As Integer = -1
                ' find paddocks with an area property set by user
                ' Loop throught all paddocks
                ' find paddock with an area set
                ' add up the total area set by user
                ' store paddock and area for latter use

                Dim SpecifiedArea = 0
                Dim TempList As New Dictionary(Of String, Double)
                'How do we get the area of indervidual paddocks?

                MyFarmArea = FarmArea
                Dim j As Integer = 0
                For Each pdk As Paddock In MasterPM.SubPaddocks
                        If (pdk.Name = "Laneways") Then
                                If (DebugLevel > 0) Then
                                        Console.WriteLine("Found lanes *******************************************************************************")
                                        Console.WriteLine(pdk.Name + "")
                                        Console.WriteLine(pdk.TypeName)
                                End If
                                Dim tempArea As Double = FarmArea * myPorportionOfFarmInLaneWays
                                SpecifiedArea += tempArea
                                MyFarmArea += tempArea 'add lane area into total area i.e. not included in effective area allocation
                                TempList.Add(pdk.Name, tempArea)
                        End If
                        j += 1
                Next

                'Todo: Get area from paddock level variable
                'For Each SubPaddock As Paddock In MasterPM.SubPaddocks
                'Dim PaddockArea As Double = SubPaddock.Variable("area").ToDouble 'throws fatal error if not declared
                'Console.WriteLine("DDRules (debug) - Paddock area == " + PaddockArea)

                'If (Not test Is Nothing) Then
                ' this approch doesn't work because paddock is not yet initilised
                'Dim a As VariableType = test.Variable("Area") 'not sure at this stage how best to do this
                'Dim a1 As Double = a.ToDouble
                'If (a1 > 0) Then
                '        TempList.Add(SubPaddock, a1)
                '        SpecifiedArea += a1
                'End If
                'End If
                'Next

                Dim UnallocatedArea = MyFarmArea - SpecifiedArea
                If (UnallocatedArea < 0) Then ' more area set manually per paddock than set by the farm component
                        'throw a fatal error'
                End If

                Dim DefaultArea As Double = 0
                Dim UnallocatedPaddocks = MasterPM.SubPaddocks.Count - TempList.Count
                If (UnallocatedPaddocks > 0) Then ' if zero then all paddock have a area attached to them. No calculations need to be done
                        DefaultArea = UnallocatedArea / UnallocatedPaddocks
                End If

                If (DebugLevel > 0) Then
                        For Each keyPair As KeyValuePair(Of String, Double) In TempList
                                Console.WriteLine(" ********* " + keyPair.Key + " area = " + keyPair.Value.ToString)
                        Next
                End If

                For Each SubPaddock As Paddock In MasterPM.SubPaddocks
                        If (DebugLevel > 0) Then
                                Console.WriteLine(SubPaddock.Name)
                                Console.WriteLine(SubPaddock.TypeName)
                        End If
                        i += 1
                        Dim TempArea As Double = DefaultArea
                        If (TempList.ContainsKey(SubPaddock.Name)) Then
                                TempArea = TempList(SubPaddock.Name).ToString
                                myLaneways = New LocalPaddockType(i, SubPaddock, TempArea)
                                'myPaddocks.Add(myLaneways)
                                MyFarmArea -= myLaneways.Area
                        Else
                                myPaddocks.Add(New LocalPaddockType(i, SubPaddock, TempArea))
                        End If

                        If (myDebugLevel > 0) Then
                                Console.WriteLine("Dubug Level = " + myDebugLevel.ToString)
                                Console.WriteLine("Predefined = " + SubPaddock.Name.ToString)
                                Console.WriteLine("i = " + i.ToString)
                                Console.WriteLine("Area = " + TempArea.ToString)
                                Console.WriteLine("   Paddock " & SubPaddock.ToString)
                                Console.WriteLine("Done.")
                        End If
                Next

                For Each pdk As LocalPaddockType In myPaddocks
                        myPaddocks2.Add(pdk.Name.ToLower, pdk)
                Next
                SilageHeap = New FeedStore
                myHerd.setValues(0, 7, 2001, Month)
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

                For Each p As LocalPaddockType In GrazedList
                        p.setJustGrazed()
                Next
                SilageHeap.Prepare()
                SupplementStore.Prepare()
        End Sub

        Public Sub Process(ByVal start_of_week As Integer)
                updateCovers()
                CheckWinteringOff()
                If Not (IsWinteringOff()) Then 'assume all stock wintering off farm i.e. no grazing
                        If (start_of_week > 0) Then
                                If AllocationType > 0 Then
                                        NewAllocation()
                                Else
                                        Allocate_Paddocks()
                                End If
                        End If

                        'If (PaddockQueue.Count = 0) Then 'either it is time to shift or have completed a full rotation
                        'Allocate_Paddocks()
                        'End If
                        Graze()
                        doAnimalsPost()
                End If
                doConservation()
                doSprayEffluient()
        End Sub

        Sub Allocate_Paddocks()
                SortPaddocksByCover()
                PaddockQueue = New Queue(Of LocalPaddockType)
                For Each Paddock As LocalPaddockType In myPaddocks
                        If (Not Paddock.Closed And Paddock.Grazable) Then
                                Paddock.GrazingCounter = Paddock.Area * myDayPerHa
                                PaddockQueue.Enqueue(Paddock) 'add all paddock to the queue (including close ones)
                        End If
                Next
                SortByIndex()
        End Sub

        Public Sub NewAllocation()

                'LUDF Process
                ' 1: Rank all paddocks by mass
                SortPaddocksByCover()
                ' 2: If in surplus then cut (some?) paddock for silage
                If (FeedSituation() > 0) Then
                        'close/cut some paddocks
                        For Each pdk As LocalPaddockType In myPaddocks
                                If (pdk.Cover > IdealPreGrazingCover()) Then 'should this cut every paddock above the line?
                                        pdk.Closed = True
                                        PaddocksClosed += 1
                                End If
                        Next
                End If
                ' 3: Allocate paddocks to meet weeks requirements
                Dim AreaToGraze As Double = FarmArea / GrazingInterval
                Dim UnallocatedArea = AreaToGraze * 7

                PaddockQueue = New Queue(Of LocalPaddockType)
                For Each Paddock As LocalPaddockType In myPaddocks
                        If (UnallocatedArea <= 0) Then
                                Exit For
                        End If
                        If (Not Paddock.Closed And Paddock.Grazable) Then
                                UnallocatedArea -= Paddock.Area
                                Paddock.GrazingCounter = Paddock.Area * myDayPerHa
                                PaddockQueue.Enqueue(Paddock) 'add all paddock to the queue (including close ones)
                        End If
                Next
                SortByIndex()
        End Sub

        Sub doSprayEffluient()
                If (end_week > 0 And myEffluentPond.Volume > 0) Then
                        Dim aList As New List(Of LocalPaddockType)
                        Dim i As Integer = Math.Round(myEffluentPaddocksPercentage * myPaddocks.Count)
                        aList = myPaddocks.GetRange(0, i)
                        Console.Out.WriteLine("Spraying dairy shed effluent to " + i.ToString() + " paddocks")
                        myEffluentIrrigator.Irrigate(myEffluentPond, aList)
                End If
        End Sub

        'Should MoveStock events be generate as part of this process?
        'If so a couple of changes need to be made to track when cows move into a new paddock. What about grazing multiple paddocks?
        Sub Graze()
                GrazedList.Clear()
                Dim PastureHarvested As Double = 0
                While (myHerd.RemainingFeedDemand > 1 And PaddockQueue.Count > 0)
                        Dim p As LocalPaddockType = PaddockQueue.Peek()
                        Dim removed As BioMass = myHerd.Graze(p, GrazingResidual)
                        PastureHarvested += removed.DM_Total
                        GrazedList.Add(p)
                        'Console.WriteLine("Grazing " & p.ApSim_ID & " DM = " & p.Cover.ToString)
                        ' deque paddock if (reached allocated time/days in paddock) or (not enough drymatter avaible to bother comming back to)
                        'If (p.GrazingCounter <= 0) Then 'p.AvalibleDryMater <= 1 Or 
                        If (p.AvalibleDryMater <= 50) Then
                                PaddockQueue.Dequeue()
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

                If (myLaneways IsNot Nothing And Not myHerd.isDry) Then
                        myHerd.doNutrientReturnsToPaddock(myLaneways, myTimeOnLaneWays)
                End If

                If (myTimeInDairyShed > 0 And Not myHerd.isDry) Then
                        myEffluentPond.Add(myHerd.getNutrientReturns(myTimeInDairyShed))
                End If

                If (GrazedList.Count > 0) Then
                        myHerd.doNutrientReturns(GrazedList)
                ElseIf (myPaddocks.Count > 0) Then 'no paddocks grazed today, return nutrients to those paddock allocated as part of the rotation
                        myHerd.doNutrientReturns(myPaddocks)
                End If
        End Sub

#Region "2: Feeding Supplements"
        'Supplementary feeding
        '<Units("kgDM")> Public SilageFed As Double 'kgDM @ 10.5me fed to meet animal requirements
        '<Units("kgDM")> Public SupplementFedout As Double 'kg of grain fed this period (to fill unsatisifed feed demand)
        Public SupplementWastage As Double = 0.0 'percentage of feed wasted as part of feeding out [Dawns' default = 10%]
        Public SilageCutWastage As Double = 0.0 'percentage of silage lost as part of cutting
        Public SilageWastage As Double = 0.0 'percentage of feed wasted as part of feeding out [Dawns' default = 10%]

        Private SupplementStore As New FeedStore 'this is used to track the supplements that have been fed to the herd (oppsite of SilageHeap)

        'Todo: Set supplemnt parameter in "Supplement heap" - this assumes same feed throughout simulation
        <Units("0-1")> Public SupplementDigestability As Double = 0.8
        <Units("0-1")> Public SilageDigestability As Double = 0.68 'need to check this value
        <Units("kgN/kgDM")> Public SilageN As Double = 0.035 'N content of silage (need to check this value - add to the user interface
        <Units("kgN/kgDM")> Public SupplementN As Double = 0.018 'N content of supplement (grain?) - add to the user interface
        <Units("ME/kgDM")> Public SupplementME As Single = 12
        <Units("ME/kgDM")> Public DefualtSilageME As Single = 10.5

        Public Property SilageME() As Double
                Get
                        Return SilageHeap.MEContent
                End Get
                Set(ByVal value As Double)
                        DefualtSilageME = value
                        If (SilageHeap.DM > 0) Then
                                SilageHeap.MEContent = value
                        End If
                End Set
        End Property


        'Silgae and Supplements will be used to completely fill the remaining demand
        'TODO - check implementation of wastage
        Sub FeedSupplements()
                If (myHerd.RemainingFeedDemand > 0) Then ' Meet any remaining demand with bought in feed (i.e. grain)
                        Dim temp As Single = FeedSilage(myHerd.RemainingFeedDemand, SilageWastage)
                        If (DebugLevel > 0) Then
                                Console.WriteLine("*** DDRules - Silage Fed *** = " + temp.ToString())
                        End If
                End If

                If (myHerd.RemainingFeedDemand > 0) Then ' Meet any remaining demand with bought in feed (i.e. grain)
                        'Dim temp As Single = FeedSupplement(myHerd.RemainingFeedDemand) '20010523 removed, SupplementME) , SupplementN, SupplementWastage, SupplementDigestability)
                        If (DebugLevel > 0) Then
                                Console.WriteLine("*** DDRules - Supplements Fed demend *** = " + myHerd.RemainingFeedDemand.ToString())
                        End If
                        Dim temp As Single = FeedSupplement(myHerd.RemainingFeedDemand, SupplementME, SupplementN, SupplementWastage, SupplementDigestability)
                        If (DebugLevel > 0) Then
                                Console.WriteLine("*** DDRules - Supplements Fed *** = " + temp.ToString())
                                Console.WriteLine("Supplment Fed Today = " + SupplementStore.ToString())
                        End If
                End If
        End Sub

        Public Property SilageStore() As Double
                Get
                        Return SilageHeap.DM
                End Get
                Set(ByVal value As Double)
                        SilageHeap = New FeedStore
                        Dim temp As BioMass = New BioMass()
                        temp.digestibility = SilageDigestability
                        temp.N_Conc = SilageN
                        temp.setME(DefualtSilageME)
                        SilageHeap.Add(temp)
                End Set
        End Property

        Function FeedSilage(ByVal MEDemand As Single, ByVal WastageFactor As Single) As Single
                Dim tempDM As BioMass = SilageHeap.Remove(MEDemand * (1 + WastageFactor))
                If (tempDM.DM_Total <= 0) Then
                        Return 0
                End If
                tempDM = tempDM.Multiply(1 - WastageFactor)
                tempDM.digestibility = SilageDigestability
                Dim tempEnergyDiff As Double = (tempDM.getME_Total - MEDemand) / MEDemand
                myHerd.Feed(tempDM, SimpleHerd.FeedType.Silage)
                Return tempDM.getME_Total
        End Function

        Function FeedSupplement(ByVal MEDemand As Single, ByVal WastageFactor As Single) As Single
                Dim dm As BioMass = New BioMass()
                Dim SupplementFedout As Double = (MEDemand / SupplementME) * (1 + SupplementWastage)
                dm.gLeaf = SupplementFedout * (1 - SupplementWastage)
                dm.setME(SupplementME)
                dm.digestibility = SupplementDigestability
                dm.N_Conc = SupplementN
                myHerd.Feed(dm, SimpleHerd.FeedType.Supplement)
                SupplementStore.Remove(dm)
                Return dm.DM_Total
        End Function

        Function FeedSupplement(ByVal MEDemand As Single, ByVal MEperKg As Single, ByVal NperKg As Single, ByVal WastageFactor As Single, ByVal Digestability As Single) As Single
                Dim dm As BioMass = New BioMass()
                Dim SupplementFedout As Double = (MEDemand / MEperKg) * (1 + WastageFactor)
                dm.gLeaf = SupplementFedout * (1 - WastageFactor)
                dm.setME(SupplementME)
                dm.digestibility = Digestability
                dm.N_Conc = NperKg
                myHerd.Feed(dm, SimpleHerd.FeedType.Supplement)
                SupplementStore.Remove(dm)
                Return dm.DM_Total
        End Function
#End Region

        Private Function IsWinteringOff() As Boolean
                Return WinterOffDryStock And myHerd.isDry
        End Function

        Public DCWO As Date 'Commence Wintering Off
        Public DSWO As Date 'Stop wintering Off
        Public PWO As Single = 1.0 'Proportion wintered off
        Private myPWO As Single = 0.0

        Private Sub CheckWinteringOff()
                'Need to adjust feed demand to exclude cows wintered off
                If isBetween(myDate, DCWO, DSWO) Then
                        myPWO = PWO
                Else
                        myPWO = 0
                End If
        End Sub

        Public Property StockingRate() As Double
                Get
                        If (IsWinteringOff()) Then
                                Return 0
                        Else
                                Return myHerd.Number_Of_Cows / FarmArea()
                        End If
                End Get
                Set(ByVal value As Double)
                        If (value < 0) Then
                                value = 0
                        End If
                        myHerd.setCowNumbers(value * FarmArea) 'assume 1ha paddocks
                End Set
        End Property

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
                                myDayPerHa = myGrazingInterval / FarmArea
                                Allocate_Paddocks()
                        End If
                End Set
        End Property

        Sub SortPaddocksByCover()
                'shufflePaddocks()
                myPaddocks.Sort(LocalPaddockType.getSortListByCover())
                If (DebugLevel > 2) Then
                        For Each lp As LocalPaddockType In myPaddocks
                                Console.Out.WriteLine(" By Cover ********* " + lp.index.ToString() + " - " + lp.Name() + " - " + lp.Cover().ToString("0"))
                        Next
                        Console.Out.WriteLine()
                End If
        End Sub

        Sub SortByIndex()
                myPaddocks.Sort(LocalPaddockType.getSortListByIndex())
                If (DebugLevel > 2) Then
                        For Each lp As LocalPaddockType In myPaddocks
                                Console.Out.WriteLine(" By Index ********* " + lp.index.ToString() + " - " + lp.Name() + " - " + lp.Cover().ToString("0"))
                        Next
                        Console.Out.WriteLine()
                End If
        End Sub

        Private Sub PrintPaddocks()
                If (myDebugLevel > 0) Then
                        For Each pdk As LocalPaddockType In myPaddocks
                                Console.WriteLine(pdk.ToString)
                        Next
                End If
        End Sub

        Public Function AverageCover() As Double
                Dim TotalCover As Double = 0
                Dim TotalArea As Double = 0
                For Each pdk As LocalPaddockType In myPaddocks
                        'For Each lp As LocalPaddockType In myPaddocks
                        '        Console.Out.WriteLine(" Average Cover ********* " + lp.index.ToString() + " - " + lp.Name() + " - " + lp.Cover().ToString("0") + " kgDM/ha - " + lp.Area.ToString("0.0") + " ha")
                        'Next
                        TotalCover += pdk.Cover * pdk.Area
                        TotalArea += pdk.Area
                Next
                Return TotalCover / TotalArea
        End Function

        Public Property FarmArea() As Double
                Get
                        Return MyFarmArea ' myPaddocks.Count ' assume 1ha paddocks for simplicity
                End Get
                Set(ByVal value As Double)

                End Set
        End Property

        Public Function getHerd() As SimpleHerd
                Return myHerd
        End Function

#Region "3: Pasture Conservation"

        <Units("kgDM/ha")> Public CDM As Double = 0 ' Conservation trigger pasture mass (Dawn default = 3500)
        <Units("kgDM/ha")> Public CR As Integer = 1500 ' Conservation cutting residual pasture mass (Dawn default)
        Private SilageHeap As New FeedStore
        Public PaddocksClosed As Integer = 0 'number of paddocks currently close for conservation
        'should these be moved out to a management script?
        Public FCD As Date 'New - First Conservation Date
        Public LCD As Date 'New - Last Conservation Date
        Public EnableSilageStore As Boolean = True 'switch ot turn off local storage of farm made silage

        Public ReadOnly Property SilageCut() As Double
                Get
                        Return SilageHeap.DMAddedToday
                End Get
        End Property

        Public ReadOnly Property SupplementFedOut() As Double
                Get
                        Return SupplementStore.DMRemovedToday
                End Get
        End Property

        Public ReadOnly Property SilageFedOut() As Double
                Get
                        Return SilageHeap.DMRemovedToday
                End Get
        End Property

        Private Sub doConservation()
                If (AllocationType = 0) Then
                        If isBetween(myDate, FCD, LCD) Then
                                ClosePaddocks()
                        End If
                End If

                Dim IsCuttingDay As Boolean = end_week > 0 'only cut once a week [as per Dawn's rules]
                If (IsCuttingDay And PaddocksClosed) Then
                        Dim HarvestedDM As BioMass = doHarvest(SilageCutWastage)
                        HarvestedDM.setME(DefualtSilageME)
                        SilageHeap.Add(HarvestedDM)
                End If
        End Sub


        Private Sub ClosePaddocks()
                updateCovers()
                For Each Paddock As LocalPaddockType In myPaddocks
                        ' if Paddock is not closed already and
                        '            is grazable i.e. not removed from the rotation and
                        '            the cover is above the trigger point (CDM) and
                        '            is not currently being grazed
                        ' then close it for silage
                        If Not (Paddock.Closed) And Paddock.Grazable And (CDM > 0) And (Paddock.Cover > CDM) And Not (GrazedList.Contains(Paddock)) Then
                                Paddock.Closed = True
                                PaddocksClosed += 1
                        End If
                Next
        End Sub

        Private Function doHarvest(ByVal loss As Double) As BioMass
                Dim result As New BioMass
                For Each Paddock As LocalPaddockType In myPaddocks
                        If (Paddock.Closed) Then                        'Harvest all closed paddocks
                                Dim CutDM As BioMass = Paddock.Harvest(CR, loss)
                                CutDM.digestibility = SilageDigestability
                                result = result.Add(CutDM)
                                If (CutDM.DM_Total > 0) Then
                                        'fire event
                                End If
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
                DM_Eaten_Silage = myHerd.DM_Eaten_Silage / FarmArea()
                DM_Eaten_Supplement = myHerd.DM_Eaten_Supplement / FarmArea()
                ME_Demand = myHerd.ME_Demand / FarmArea()
                ME_Eaten = myHerd.ME_Eaten / FarmArea()
                ME_Eaten_Pasture = myHerd.ME_Eaten_Pasture / FarmArea()
                ME_Eaten_Silage = myHerd.ME_Eaten_Silage / FarmArea()
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
        <Output()> <Units("MJME/ha")> Public ME_Eaten_Silage As Single
        <Output()> <Units("MJME/ha")> Public ME_Eaten_Supplement As Single
        <Output()> <Units("kgDM/ha")> Public DM_Eaten As Single
        <Output()> <Units("kgDM/ha")> Public DM_Eaten_Pasture As Single
        <Output()> <Units("kgDM/ha")> Public DM_Eaten_Silage As Single
        <Output()> <Units("kgDM/ha")> Public DM_Eaten_Supplement As Single
        <Output()> <Units("kgN/ha")> Public N_Eaten As Single
        <Output()> <Units("kgN/ha")> Public N_Eaten_Pasture As Single
        <Output()> <Units("kgN/ha")> Public N_Eaten_Supplement As Single
        <Output()> <Units("kgN/ha")> Public N_to_milk As Single
        <Output()> <Units("kgN/ha")> Public N_to_BC As Single
        <Output()> <Units("kgN/ha")> Public N_to_feaces As Single
        <Output()> <Units("kgN/ha")> Public N_to_urine As Single
        <Output()> <Units("kgN/ha")> Public DM_to_feaces As Single ' added

        <Output()> <Units("")> Public ReadOnly Property PaddockStatus() As String()
                Get
                        Dim result(myPaddocks.Count - 1) As String
                        'sort by index
                        For i As Integer = 0 To (myPaddocks.Count - 1)
                                result(i) = myPaddocks(i).StatusCode
                        Next
                        Return result
                End Get
        End Property

        Public Property PaddockGrazable(ByVal i As Integer) As Boolean
                Get
                        If (i >= 0 And i < myPaddocks.Count) Then
                                Dim p As LocalPaddockType = myPaddocks(i)
                                Return p.Grazable
                        Else
                                Return False 'not a paddock in the simulation? Or not know to DDRules atleast!
                        End If
                End Get
                Set(ByVal value As Boolean)
                        If (i >= 0 And i < myPaddocks.Count) Then
                                Dim p As LocalPaddockType = myPaddocks(i)
                                p.Grazable = value
                        End If
                End Set
        End Property

        ' by paddock variables
        Public ReadOnly Property DM_Eaten_Pdks() As Single()
                Get
                        Dim result(myPaddocks.Count - 1) As Single
                        'sort by index here
                        For i As Integer = 0 To (myPaddocks.Count - 1)
                                result(i) = myPaddocks(i).DM_Eaten()
                        Next
                        Return result
                End Get
        End Property

        Public ReadOnly Property DM_Eaten_Pdks_Ha() As Single()
                Get
                        Dim result() As Single = DM_Eaten_Pdks
                        'sort by index here
                        For i As Integer = 0 To (result.Length - 1)
                                result(i) /= myPaddocks(i).Area
                        Next
                        Return result
                End Get
        End Property

        <Output()> <Units("MJME/ha")> Public ReadOnly Property AverageGrowthRate() As Single()
                Get
                        Dim result(myPaddocks.Count - 1) As Single
                        'sort by index here
                        For i As Integer = 0 To (myPaddocks.Count - 1)
                                result(i) = myPaddocks(i).AverageGrowthRate()
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

        <Output()> <Units("0-1")> Public Property PorportionOfFarmInLaneWays() As Double
                Get
                        Return myPorportionOfFarmInLaneWays
                End Get
                Set(ByVal value As Double)
                        myPorportionOfFarmInLaneWays = value
                End Set
        End Property

        <Output()> <Units("0-24")> Public Property HoursOnLaneWays() As Double
                Get
                        Return myTimeOnLaneWays * 24.0
                End Get
                Set(ByVal value As Double)
                        If (value < 0) Then
                                value = 0
                        End If
                        If value > 24 Then
                                value = 24
                        End If
                        myTimeOnLaneWays = value / 24.0
                End Set
        End Property

        <Output()> <Units("0-24")> Public Property HoursInDairyShed() As Double
                Get
                        Return myTimeInDairyShed * 24.0
                End Get
                Set(ByVal value As Double)
                        If (value < 0) Then
                                value = 0
                        End If
                        If value > 24 Then
                                value = 24
                        End If
                        myTimeInDairyShed = value / 24.0
                End Set
        End Property


        Public Property DebugLevel() As Integer
                Get
                        Return myDebugLevel
                End Get
                Set(ByVal value As Integer)
                        myDebugLevel = value
                        If Not (myPaddocks Is Nothing) Then
                                For Each paddock As LocalPaddockType In myPaddocks
                                        paddock.DebugLevel = value - 1
                                Next
                        End If
                End Set
        End Property

        Public Sub TestFeedWedge(ByVal post As Integer)
                SortPaddocksByCover()
                For Each pdk As LocalPaddockType In myPaddocks

                Next
                SortByIndex()
        End Sub

        Public Function FeedSituation() As Double
                If (myHerd.Number_Of_Cows > 0) Then
                        Dim post = GrazingResidual
                        Dim target As Double = IdealGrazingCover()
                        Dim pre As Double = post + (target - post)
                        Return AverageCover() - target
                Else
                        Return 0
                End If
        End Function

        ' Source: DairyNZ - Feed Wedge Reconer
        Public Function IdealGrazingCover() As Double
                Dim Cows_ha As Double = StockingRate
                Dim KgDM_Cow As Double = myHerd.ME_Demand_Cow / 12.0 '17 'from doc
                Dim RotationLength As Double = GrazingInterval
                Dim TargetResidual As Double = GrazingResidual
                Dim KgDM_ha As Double = Cows_ha * KgDM_Cow * RotationLength + TargetResidual
                Return KgDM_ha
        End Function

        Public Function IdealPreGrazingCover() As Double
                Dim post As Double = GrazingResidual
                Dim average As Double = IdealGrazingCover()
                Return average + (average - post)
        End Function

        Public Function PreGrazingCover() As Double
                Return 0 'need to store this somehow
                'myPreGraze = Max(myPreGraze, currentPdk.Cover)
                'onPrepare -> myPregraze = 0
        End Function

        Public Function Fertilise(ByVal data As FertiliserApplicationType) As Double
                If (data.Amount > 0) Then
                        Dim total As Double = 0
                        Dim totalArea As Double = 0
                        For Each pdk As LocalPaddockType In myPaddocks
                                pdk.Apply(data)
                                totalArea += pdk.Area
                                total += (data.Amount * pdk.Area)
                        Next
                        'Console.WriteLine("Fertilise {0:d} / {1:d} = {2:d}", total, FarmArea, total / FarmArea)
                        If (DebugLevel > 0) Then
                                Console.WriteLine("Fertilise " + total.ToString() + " / " + FarmArea.ToString() + " = " + (total / FarmArea).ToString())
                        End If
                        Return total / totalArea
                Else
                        Return 0.0
                End If
        End Function

        '<Units("mm/ha")> 
        Function Irrigate(ByVal data As IrrigationApplicationType) As Double
                If (data.Amount > 0) Then
                        Dim total As Double = 0
                        Dim area As Double = 0
                        For Each paddock As LocalPaddockType In myPaddocks
                                paddock.Irrigate(data)
                                area += data.Crop_Area
                                total += (data.Amount * data.Crop_Area)
                        Next
                        If (DebugLevel > 0) Then
                                Console.WriteLine(total / FarmArea)
                        End If
                        Return total / area
                Else
                        Return 0.0
                End If
        End Function

        <Output()> <Units("")> Public ReadOnly Property PlantAvalibleWater(ByVal atDepth As Single) As Single
                Get
                        Dim total As Double = 0
                        Dim area As Double = 0
                        For Each paddock As LocalPaddockType In myPaddocks
                                Dim swd As Double = paddock.PlantAvalibleWater(atDepth)
                                area += paddock.Area
                                total += (swd * paddock.Area)
                        Next
                        Return total / area
                End Get
        End Property

        'Proportion of farm to return effluient to
        <Param()> <Units("0-1")> Public Property EffluentPaddocksPercentage() As Double
                Get
                        Return myEffluentPaddocksPercentage
                End Get
                Set(ByVal value As Double)
                        If (value > 1.0) Then
                                value = 1.0
                        ElseIf (value < 0.0) Then
                                value = 0.0
                        End If
                        myEffluentPaddocksPercentage = value
                End Set
        End Property

        Public Sub setMilkSolids(ByVal values As Double())
                'Todo 20110524 - add checking here
                myHerd.setMilkSolids(values)
        End Sub

        Public Sub setLiveWeight(ByVal values As Double())
                'Todo 20110524 - add checking here
                myHerd.setLiveWeight(values)
        End Sub

        Dim effluentPaddocks As List(Of LocalPaddockType) = New List(Of LocalPaddockType)

        Sub setEffluentPaddocks(ByVal values As String())
                effluentPaddocks = New List(Of LocalPaddockType)(values.Length)
                For Each strPaddockName As String In values
                        If (myPaddocks2.ContainsKey(strPaddockName)) Then
                                Dim p As LocalPaddockType = myPaddocks2(strPaddockName)
                                Console.WriteLine(p)
                                effluentPaddocks.Add(myPaddocks2(strPaddockName))
                        End If
                Next
        End Sub

        Function getEffluentPaddocks() As String()
                Dim result(effluentPaddocks.Count) As String
                Dim i As Integer = 0
                For Each pdk As LocalPaddockType In effluentPaddocks
                        result(i) = pdk.Name
                        i += 1
                Next
                Return result
        End Function

End Class
