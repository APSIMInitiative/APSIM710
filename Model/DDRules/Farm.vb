Imports ModelFramework

Public Class Farm
    Inherits Instance
    Private myDebugLevel As Integer = 0
    Private myPaddocks As List(Of LocalPaddockType)         ' Full list of apsim paddocks
    Private myPaddocks2 As Dictionary(Of String, LocalPaddockType)         ' Full list of apsim paddocks
    Private myMilkingHerd As SimpleHerd                            ' Dairy herd / on milking platform
    Private myDryCowHerd As SimpleHerd                            ' Dry Cow herd / on or off milking platform
    Private MyFarmArea As Double

    Dim PaddockQueue As Queue(Of LocalPaddockType)
    Private GrazedList As List(Of LocalPaddockType)         ' List of grazed paddocks
    Private myGrazingResidual As Integer = 1600
    Private myGrazingInterval As Integer = 30
    Private myDayPerHa As Double 'this value is used to control rotation speed
    Public WinterOffDryStock As Boolean = True

    Private myDate As Date
    Private Day As Integer
    Private Month As Integer
    Private end_week As Boolean
    Private myPorportionOfFarmInLaneWays As Double = 0
    Private myTimeOnLaneWays As Double = 0
    Private myTimeInDairyShed As Double = 0
    Public myEffluentPaddocksPercentage As Double = 1.0 '[default = 1.0 = spread to all paddocks]

    Public myEffluentPond As New EffluentPond
    Public myEffluentIrrigator As New EffluentIrrigator
    Public AllocationType As Integer = 0
    'LUDF Process
    Private EnableCutting As Boolean = True
    Private DefaultPastureME As Double = 11.5 '12.3 average me/kgDM from 2006-2010
    Public myAverageCover As MovingAverage = New MovingAverage(7)

    Public Sub New()
        myMilkingHerd = New SimpleHerd()
        myDryCowHerd = New SimpleHerd()
        myPaddocks2 = New Dictionary(Of String, LocalPaddockType)
    End Sub

    Public Sub Init(ByVal MasterPM As Paddock, ByVal Year As Integer, ByVal Month As Integer, ByVal FarmArea As Double)
        If (DebugLevel > 0) Then
            Console.WriteLine("DDRules.Farm.Init()")
            Console.WriteLine("   MasterPM = " + MasterPM.ToString())
            Console.WriteLine("   Year     = " + Year.ToString())
            Console.WriteLine("   Month    = " + Month.ToString())
            Console.WriteLine("   FarmArea = " + FarmArea.ToString())
        End If
        myPaddocks = New List(Of LocalPaddockType)
        PaddockQueue = New Queue(Of LocalPaddockType)
        GrazedList = New List(Of LocalPaddockType)
        Dim i As Integer = -1
        ' find paddocks with an area property set by user
        ' Loop throught all paddocks
        ' find paddock with an area set
        ' add up the total area set by user
        ' store paddock and area for latter use

        Dim SpecifiedArea As Double = 0
        Dim TempList As New Dictionary(Of String, Double)
        'How do we get the area of indervidual paddocks?

        MyFarmArea = FarmArea
        Dim j As Integer = 0
        'If (DebugLevel > 0) Then
        '        Console.WriteLine("DDRules.Farm.Init() - Checking for laneways")
        'End If
        'For Each pdk As Paddock In MasterPM.SubPaddocks
        '        If (pdk.Name = "Laneways") Then
        '                If (DebugLevel > 0) Then
        '                        Console.WriteLine("Found lanes *******************************************************************************")
        '                        Console.WriteLine(pdk.Name + "")
        '                        Console.WriteLine(pdk.TypeName)
        '                End If
        '                Dim tempArea As Double = FarmArea * myPorportionOfFarmInLaneWays
        '                SpecifiedArea += tempArea
        '                MyFarmArea += tempArea 'add lane area into total area i.e. not included in effective area allocation
        '                TempList.Add(pdk.Name, tempArea)
        '        End If
        '        j += 1
        'Next
        'If (DebugLevel > 0) Then
        '        Console.WriteLine("DDRules.Farm.Init() - Checking for laneways - Complete")
        'End If

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

        Dim UnallocatedArea As Double = MyFarmArea - SpecifiedArea
        If (UnallocatedArea < 0) Then ' more area set manually per paddock than set by the farm component
            'throw a fatal error'
        End If

        Dim DefaultArea As Double = 0
        Dim UnallocatedPaddocks As Integer = MasterPM.SubPaddocks.Count - TempList.Count
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
            'If (TempList.ContainsKey(SubPaddock.Name)) Then
            '    TempArea = TempList(SubPaddock.Name).ToString
            '    myLaneways = New LocalPaddockType(i, SubPaddock, TempArea)
            '    myPaddocks.Add(myLaneways)
            '    MyFarmArea -= myLaneways.Area
            'Else
            Dim pdk As New LocalPaddockType(i, SubPaddock, TempArea)
            myPaddocks.Add(pdk)
            'End If

            If (myDebugLevel > 0) Then
                Console.WriteLine("Dubug Level = " & myDebugLevel)
                Console.WriteLine("Predefined = " & SubPaddock.Name)
                Console.WriteLine("i = " & i)
                Console.WriteLine("Area = " & TempArea)
                Console.WriteLine("   Paddock " & SubPaddock.ToString)
                Console.WriteLine("   Cover " & pdk.Cover.ToString("0"))
                Console.WriteLine("Done.")
            End If
        Next

        For Each pdk As LocalPaddockType In myPaddocks
            myPaddocks2.Add(pdk.Name.ToLower, pdk)
        Next

        If (SilageHeap Is Nothing) Then
            SilageHeap = New FeedStore
        End If

        myMilkingHerd.setValues(7, Year, Month)
    End Sub

    Public Sub Prepare(ByVal Year As Integer, ByVal Month As Integer, ByVal Day As Integer, ByVal end_week As Boolean)
        Me.Day = Day
        Me.Month = Month
        myDate = New Date(Year, Month, Day)
        Me.end_week = end_week
        For Each Paddock As LocalPaddockType In myPaddocks
            Paddock.OnPrepare()
        Next

        myMilkingHerd.onPrepare(Year, Month)

        'For Each p As LocalPaddockType In GrazedList
        '        p.setJustGrazed()
        'Next
        SilageHeap.Prepare()
        SupplementStore.Prepare()

        preGrazeCovers = updateCovers()
        postGrazeCovers = updateCovers()
    End Sub

    Public Function Update() As Double()
        Dim result() As Double = updateCovers()
        Return result
    End Function

    Public preGrazeCovers() As Double
    Public postGrazeCovers() As Double
    Public Sub Process(ByVal start_of_week As Integer)
        CheckWinteringOff()
        If Not (IsWinteringOff()) Then 'assume all stock wintering off farm i.e. no grazing
            If (start_of_week) Then
                Select Case AllocationType
                    Case 1 : NewAllocation()
                    Case Else
                        Allocate_Paddocks()
                End Select
            End If

            If (DebugLevel > 1) Then
                Console.WriteLine(" DDRules - Grazing paddock queue : Day = " & start_of_week.ToString)
                For Each pdk As LocalPaddockType In PaddockQueue
                    Console.WriteLine("    " & pdk.ToString())
                Next
                Console.WriteLine(" DDRules - Grazing paddock queue - done")
            End If

            'If (PaddockQueue.Count = 0) Then 'either it is time to shift or have completed a full rotation
            'Allocate_Paddocks()
            'End If
            preGrazeCovers = updateCovers()
            Graze()
            postGrazeCovers = updateCovers()
            doAnimalsPost()
        End If
        doConservation()
        doSprayEffluient()

        For Each Paddock As LocalPaddockType In myPaddocks
            Paddock.OnPost()
        Next
        myAverageCover.Add(AverageCover)
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

        ' 3: Allocate paddocks to meet weeks requirements
        Dim AreaToGraze As Double = FarmArea / GrazingInterval
        Dim UnallocatedArea As Double = AreaToGraze * 7

        'First allocate paddock that need to be returned to
        Dim TempList As List(Of LocalPaddockType) = New List(Of LocalPaddockType)

        'Need to keep tabs on paddocks currently being grazed i.e not down to residual or for holding long 
        For Each pdk As LocalPaddockType In myPaddocks
            If (pdk.BeingGrazed()) Then
                pdk.GrazingCounter = Math.Min(pdk.GrazingCounter, pdk.Area * myDayPerHa)
                TempList.Add(pdk)
                'UnallocatedArea -= pdk.Area 'to count or not to count...that is the question
            End If
        Next

        TempList.Sort(LocalPaddockType.getSortListByCover(True))

        PaddockQueue = New Queue(Of LocalPaddockType)
        For Each pdk As LocalPaddockType In TempList
            If (DebugLevel > 2) Then
                Console.WriteLine("  DDRules - re-adding paddock to graze down properly " & pdk.ToString())
            End If
            PaddockQueue.Enqueue(pdk)
        Next

        'Remove any surplus
        'If (EnableCutting) Then
        '        SortPaddocksByCover() '1: Rank all paddocks by mass
        '        If (FeedSituation() > 0) Then '2: If in surplus then cut (some?) paddock for silage
        '                For Each pdk As LocalPaddockType In myPaddocks
        '                        If (Not pdk.BeingGrazed And pdk.Cover > IdealPreGrazingCover()) Then 'should this cut every paddock above the line?
        '                                pdk.Closed = True
        '                                PaddocksClosed += 1
        '                        End If
        '                Next
        '        End If
        'End If

        SortPaddocksByCover() 'Rank all paddocks again by mass
        For Each pdk As LocalPaddockType In myPaddocks
            If (UnallocatedArea <= 0) Then
                Exit For
            End If
            'if paddock isn't...closed for silage cutting, is able to be grazed and not already added above then
            If (Not pdk.Closed And pdk.Grazable And Not PaddockQueue.Contains(pdk)) Then
                UnallocatedArea -= pdk.Area
                pdk.GrazingCounter = pdk.Area * myDayPerHa
                PaddockQueue.Enqueue(pdk)
            End If
        Next

        If (DebugLevel > 1) Then
            Console.WriteLine(" DDRules - Grazing paddock queue")
            For Each pdk As LocalPaddockType In PaddockQueue
                Console.WriteLine("    " & pdk.ToString())
            Next
            Console.WriteLine(" DDRules - Grazing paddock queue - done")
        End If
        SortByIndex()
    End Sub

    'Should MoveStock events be generate as part of this process?
    'If so a couple of changes need to be made to track when cows move into a new paddock. What about grazing multiple paddocks?
    Sub Graze()
        GrazedList.Clear()
        Dim PastureHarvested As Double = 0
        While (myMilkingHerd.RemainingFeedDemand > 1 And PaddockQueue.Count > 0)
            Dim p As LocalPaddockType = PaddockQueue.Peek()
            Dim removed As BioMass = myMilkingHerd.Graze(p, GrazingResidual)
            PastureHarvested += removed.DM_Total
            GrazedList.Add(p)
            'Console.WriteLine("Grazing " & p.ApSim_ID & " DM = " & p.Cover.ToString)
            ' deque paddock if (reached allocated time/days in paddock) or (not enough drymatter avaible to bother comming back to)
            'If (p.GrazingCounter <= 0) Then 'p.AvalibleDryMater <= 1 Or 
            If (p.GrazingCounter <= 0) Then
                '                        If (p.AvalibleDryMater <= 50) Then
                Dim pdk As LocalPaddockType = PaddockQueue.Dequeue()
                pdk.JustGrazed = True
                'PaddockQueue.Enqueue(pdk)
            Else
                Return
            End If
        End While
    End Sub

    Public Function CutToFeedWedge(ByVal Optimum_residual As Integer, ByVal Rotation As Integer, Optional ByVal Stocking_rate As Double = 3.0, Optional ByVal Intake As Double = 18.0) As BioMass '
        Dim pre As Integer = CalcPregrazingCoverTarget(Stocking_rate, Intake, Rotation, Optimum_residual)
        Dim interval As Double = (pre - Optimum_residual) / (myPaddocks.Count)
        Dim i As Integer = 0
        Dim result As BioMass = New BioMass()

        updateCovers()
        For Each pdk As LocalPaddockType In myPaddocks
            If Not (myLanewayPaddocks.Contains(pdk)) Then 'don't include laneway paddocks
                Dim cover As Integer = Optimum_residual + (i * interval)
                Dim temp As BioMass = pdk.Harvest(cover, SilageCutWastage)
                result = result.Add(temp)
                i += 1
            End If
        Next
        Return result
    End Function

    Private Sub doAnimalsPost()
        If (myMilkingHerd.isUnderFed) Then
            FeedSupplements()
        End If

        myMilkingHerd.doNitrogenPartioning()

        If (myLanewayPaddocks.Count > 0 And Not myMilkingHerd.isDry) Then
            myMilkingHerd.doNutrientReturnsToPaddock(myLanewayPaddocks, myTimeOnLaneWays)
        End If

        If (myTimeInDairyShed > 0 And Not myMilkingHerd.isDry) Then
            myEffluentPond.Add(myMilkingHerd.getNutrientReturns(myTimeInDairyShed))
        End If

        If (GrazedList.Count > 0) Then
            myMilkingHerd.doNutrientReturns(GrazedList)
        ElseIf (myPaddocks.Count > 0) Then 'no paddocks grazed today, return nutrients to those paddock allocated as part of the rotation
            myMilkingHerd.doNutrientReturns(myPaddocks)
        End If
    End Sub

#Region "2: Feeding Supplements"
    'Supplementary feeding
    '<Units("kgDM")> Public SilageFed As Double 'kgDM @ 10.5me fed to meet animal requirements
    '<Units("kgDM")> Public SupplementFedout As Double 'kg of grain fed this period (to fill unsatisifed feed demand)
    Public SupplementWastage As Double = 0.0 'percentage of feed wasted as part of feeding out [Dawns' default = 10%]
    Public SilageCutWastage As Double = 0.05 'percentage of silage lost as part of cutting 5% @ 30%DM : farmFact 1-44 Losses when making pasture silage
    Public SilageWastage As Double = 0.15 'percentage of feed wasted as part of feeding out [Dawns' default = 15%]

    Private SupplementStore As New FeedStore 'this is used to track the supplements that have been fed to the herd (oppsite of SilageHeap)

    'Todo: Set supplemnt parameter in "Supplement heap" - this assumes same feed throughout simulation
    Public SupplementDigestability As Double = 0.8
    Public SilageDigestability As Double = 0.68 'need to check this value
    Public SilageQualityModifier As Double = 0.9 'need to check this value

    Public SilageN As Double = 0.035 'N content of silage (need to check this value - add to the user interface
    Public SupplementN As Double = 0.018 'N content of supplement (grain?) - add to the user interface
    Public SupplementME As Single = 12
    Public DefualtSilageME As Single = 10.5

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
        If (myMilkingHerd.RemainingFeedDemand > 0) Then ' Meet any remaining demand with bought in feed (i.e. grain)
            Dim temp As Single = FeedSilage(myMilkingHerd.RemainingFeedDemand, SilageWastage)
            If (DebugLevel > 0) Then
                Console.WriteLine("*** DDRules - Silage Fed *** = " + temp.ToString())
            End If
        End If

        If (myMilkingHerd.RemainingFeedDemand > 0) Then ' Meet any remaining demand with bought in feed (i.e. grain)
            'Dim temp As Single = FeedSupplement(myHerd.RemainingFeedDemand) '20010523 removed, SupplementME) , SupplementN, SupplementWastage, SupplementDigestability)
            If (DebugLevel > 0) Then
                Console.WriteLine("*** DDRules - Supplements Fed demend *** = " + myMilkingHerd.RemainingFeedDemand.ToString())
            End If
            Dim temp As Single = FeedSupplement(myMilkingHerd.RemainingFeedDemand, SupplementME, SupplementN, SupplementWastage, SupplementDigestability)
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
            temp.set(value, SilageDigestability, SilageN, DefualtSilageME)
            'temp.digestibility = SilageDigestability
            'temp.N_Conc = SilageN
            'temp.setME(DefualtSilageME)
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
        tempDM.N_Conc = SilageN
        Dim tempEnergyDiff As Double = (tempDM.getME_Total - MEDemand) / MEDemand
        myMilkingHerd.Feed(tempDM, SimpleHerd.FeedType.Silage)
        Return tempDM.getME_Total
    End Function

    Function FeedSupplement(ByVal MEDemand As Single, ByVal WastageFactor As Single) As Single
        Dim dm As BioMass = New BioMass()
        Dim SupplementFedout As Double = (MEDemand / SupplementME) * (1 + SupplementWastage)
        dm.gLeaf = SupplementFedout * (1 - SupplementWastage)
        dm.setME(SupplementME)
        dm.digestibility = SupplementDigestability
        dm.N_Conc = SupplementN
        myMilkingHerd.Feed(dm, SimpleHerd.FeedType.Supplement)
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
        myMilkingHerd.Feed(dm, SimpleHerd.FeedType.Supplement)
        SupplementStore.Remove(dm)
        Return dm.DM_Total
    End Function
#End Region

    Private Function IsWinteringOff() As Boolean
        Return WinterOffDryStock And myMilkingHerd.isDry
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
                Return myMilkingHerd.Size() / FarmArea()
            End If
        End Get
        Set(ByVal value As Double)
            If (value < 0) Then
                value = 0
            End If
            Dim temp As Double = DryOffProportion
            myMilkingHerd.setCowNumbers(0)
            myDryCowHerd.setCowNumbers(0)
            myMilkingHerd.setCowNumbers(value * FarmArea) 'assume 1ha paddocks
            DryOffProportion = temp
        End Set
    End Property

    Function updateCovers() As Double()
        Dim result(myPaddocks.Count) As Double
        Dim i As Integer = 0
        For Each Paddock As LocalPaddockType In myPaddocks
            Paddock.UpdateCovers()
            result(i) = Paddock.Cover
        Next
        Return result
    End Function

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
                If AllocationType > 0 Then
                    NewAllocation()
                Else
                    Allocate_Paddocks()
                End If
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
            TotalCover += pdk.Cover() * pdk.Area
            TotalArea += pdk.Area
        Next
        Return TotalCover / TotalArea
    End Function

    Public Function AverageFarmCoverWeekly() As Double
        Return myAverageCover.Average()
    End Function

    Public Property FarmArea() As Double
        Get
            Return MyFarmArea ' myPaddocks.Count ' assume 1ha paddocks for simplicity
        End Get
        Set(ByVal value As Double)
            MyFarmArea = value
        End Set
    End Property

    Public Function getHerd() As SimpleHerd
        Return myMilkingHerd
    End Function

#Region "3: Pasture Conservation"

    <Units("kgDM/ha")> Public CDM As Double = 0 ' Conservation trigger pasture mass (Dawn default = 3500)
    <Units("kgDM/ha")> Public CR As Integer = 1500 ' Conservation cutting residual pasture mass (Dawn default)
    Private SilageHeap As New FeedStore
    Public PaddocksClosed As Integer = 0 'number of paddocks currently close for conservation
    'should these be moved out to a management script?
    Public FCD As Date 'New - First Conservation Date
    Public LCD As Date 'New - Last Conservation Date
    Public myEnableSilageStore As Boolean = True 'switch ot turn off local storage of farm made silage

    Public Property EnableSilageStore() As Boolean
        Get
            Return myEnableSilageStore
        End Get
        Set(ByVal value As Boolean)
            myEnableSilageStore = value
            SilageHeap.doStore = myEnableSilageStore
        End Set
    End Property

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
        Dim IsCuttingDay As Boolean = end_week 'only cut once a week [as per Dawn's rules]
        If isBetween(myDate, FCD, LCD) Then
            Select Case AllocationType
                Case 1 : IsCuttingDay = True
                Case Else : ClosePaddocks()
            End Select
        End If

        If (IsCuttingDay) Then ' And PaddocksClosed) Then
            Dim HarvestedDM As BioMass = New BioMass()
            Select Case (AllocationType)
                Case 1
                    If (FeedSituation() / IdealGrazingCover() > 0.2) Then 'Cut when 10% in surplus?
                        HarvestedDM = CutToFeedWedge(myGrazingResidual, myGrazingInterval, MilkingCows() / FarmArea, myMilkingHerd.ME_Demand_Cow / DefaultPastureME)
                    End If
                Case Else
                    HarvestedDM = doHarvest(SilageCutWastage)
            End Select

            If (DebugLevel > 0) Then
                Console.WriteLine("DDRules Harvested " & HarvestedDM.ToString())
            End If
            'HarvestedDM.setME(DefualtSilageME)
            HarvestedDM.setME(HarvestedDM.getME() * SilageQualityModifier)
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
        DM_Eaten = myMilkingHerd.DM_Eaten / FarmArea()
        DM_Eaten_Pasture = myMilkingHerd.DM_Eaten_Pasture / FarmArea()
        DM_Eaten_Silage = myMilkingHerd.DM_Eaten_Silage / FarmArea()
        DM_Eaten_Supplement = myMilkingHerd.DM_Eaten_Supplement / FarmArea()
        ME_Demand = myMilkingHerd.ME_Demand / FarmArea()
        ME_Eaten = myMilkingHerd.ME_Eaten / FarmArea()
        ME_Eaten_Pasture = myMilkingHerd.ME_Eaten_Pasture / FarmArea()
        ME_Eaten_Silage = myMilkingHerd.ME_Eaten_Silage / FarmArea()
        ME_Eaten_Supplement = myMilkingHerd.ME_Eaten_Supplement / FarmArea()
        N_Eaten = myMilkingHerd.N_Eaten / FarmArea()
        N_Eaten_Pasture = myMilkingHerd.N_Eaten_Pasture / FarmArea()
        N_Eaten_Silage = myMilkingHerd.N_Eaten_Silage / FarmArea()
        N_Eaten_Supplement = myMilkingHerd.N_Eaten_Supplement / FarmArea()
        N_to_milk = myMilkingHerd.N_to_Milk / FarmArea()
        N_to_BC = myMilkingHerd.N_to_BC / FarmArea()
        N_to_feaces = myMilkingHerd.N_to_feaces / FarmArea()
        DM_to_feaces = myMilkingHerd.DM_to_feaces / FarmArea()
        N_to_urine = myMilkingHerd.N_to_urine / FarmArea()
        N_Balance = myMilkingHerd.N_Balance / FarmArea()
        N_Out = myMilkingHerd.N_Out / FarmArea()

        ME_Demand_Cow = myMilkingHerd.ME_Demand_Cow()
        ME_Eaten_Cow = myMilkingHerd.ME_Eaten_Cow()
        ME_Eaten_Pasture_Cow = myMilkingHerd.ME_Eaten_Pasture_Cow()
        ME_Eaten_Supplement_Cow = myMilkingHerd.ME_Eaten_Supplement_Cow()
        DM_Eaten_Cow = myMilkingHerd.DM_Eaten_Cow()
        DM_Eaten_Pasture_Cow = myMilkingHerd.DM_Eaten_Pasture_Cow()
        DM_Eaten_Supplement_Cow = myMilkingHerd.DM_Eaten_Supplement_Cow()
        N_Eaten_Cow = myMilkingHerd.N_Eaten_Cow()
        N_Eaten_Pasture_Cow = myMilkingHerd.N_Eaten_Pasture_Cow()
        N_Eaten_Supplement_Cow = myMilkingHerd.N_Eaten_Supplement_Cow()
        N_to_milk_Cow = myMilkingHerd.N_to_milk_Cow()
        N_to_BC_Cow = myMilkingHerd.N_to_BC_Cow()
        N_to_feaces_Cow = myMilkingHerd.N_to_feaces_Cow()
        N_to_urine_Cow = myMilkingHerd.N_to_urine_Cow()
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
    Public N_Balance As Single
    Public N_Out As Single

    Public ME_Demand As Single
    Public ME_Eaten As Single
    Public ME_Eaten_Pasture As Single
    Public ME_Eaten_Silage As Single
    Public ME_Eaten_Supplement As Single
    Public DM_Eaten As Single
    Public DM_Eaten_Pasture As Single
    Public DM_Eaten_Silage As Single
    Public DM_Eaten_Supplement As Single
    Public N_Eaten As Single
    Public N_Eaten_Pasture As Single
    Public N_Eaten_Silage As Single
    Public N_Eaten_Supplement As Single
    Public N_to_milk As Single
    Public N_to_BC As Single
    Public N_to_feaces As Single
    Public N_to_urine As Double
    Public DM_to_feaces As Single ' added

    Public ReadOnly Property PaddockStatus() As String()
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

    Public ReadOnly Property AverageGrowthRate() As Single
        Get
            Dim growth As Single = 0
            Dim area As Single = 0
            'sort by index here
            For Each pdk As LocalPaddockType In myPaddocks
                growth += pdk.AverageGrowthRate() * pdk.Area
                area += pdk.Area
            Next
            Return growth / area
        End Get
    End Property

    '<Output()> <Units("MJME/ha")> Public ReadOnly Property AverageGrowthRate() As Single()
    '        Get
    '                Dim result(myPaddocks.Count - 1) As Single
    '                'sort by index here
    '                For i As Integer = 0 To (myPaddocks.Count - 1)
    '                        result(i) = myPaddocks(i).AverageGrowthRate()
    '                Next
    '                Return result
    '        End Get
    'End Property

    Public ReadOnly Property ME_Eaten_Pasture_Pdks() As Single()
        Get
            Dim result(myPaddocks.Count - 1) As Single
            'sort by index here
            For i As Integer = 0 To (myPaddocks.Count - 1)
                result(i) = myPaddocks(i).ME_Eaten()
            Next
            Return result
        End Get
    End Property
    Public ME_Eaten_Supplement_Pdks As Single()
    Public ReadOnly Property ME_Eaten_Pdks() As Single()
        Get
            Dim result(myPaddocks.Count - 1) As Single
            'sort by index
            For i As Integer = 0 To (myPaddocks.Count - 1)
                result(i) = myPaddocks(i).ME_Eaten()
            Next
            Return result
        End Get
    End Property
    Public DM_Eaten_Pasture_Pdks As Single()
    Public DM_Eaten_Supplement_Pdks As Single()
    Public ReadOnly Property N_Eaten_Pdks() As Single()
        Get
            Dim result(myPaddocks.Count - 1) As Single
            'sort by index
            For i As Integer = 0 To (myPaddocks.Count - 1)
                result(i) = myPaddocks(i).N_Eaten()
            Next
            Return result
        End Get
    End Property
    Public N_Eaten_Pasture_Pdks As Single()
    Public N_Eaten_Supplement_Pdks As Single()
    Public N_to_milk_Pdks As Single()
    Public N_to_BC_Pdks As Single()
    Public ReadOnly Property N_to_feaces_Pdks() As Single()
        Get
            Dim result(myPaddocks.Count - 1) As Single
            'sort by index
            For i As Integer = 0 To (myPaddocks.Count - 1)
                result(i) = myPaddocks(i).N_From_Feaces()
            Next
            Return result
        End Get
    End Property
    Public ReadOnly Property N_to_urine_Pdks() As Double()
        Get
            Dim result(myPaddocks.Count - 1) As Double
            'sort by index
            For i As Integer = 0 To (myPaddocks.Count - 1)
                result(i) = myPaddocks(i).N_Eaten()
            Next
            Return result
        End Get
    End Property
    Public ME_Demand_Cow As Single
    Public ME_Eaten_Cow As Single
    Public ME_Eaten_Pasture_Cow As Single
    Public ME_Eaten_Supplement_Cow As Single
    Public DM_Eaten_Cow As Single
    Public DM_Eaten_Pasture_Cow As Single
    Public DM_Eaten_Supplement_Cow As Single
    Public N_Eaten_Cow As Single
    Public N_Eaten_Pasture_Cow As Single
    Public N_Eaten_Supplement_Cow As Single
    Public N_to_milk_Cow As Single
    Public N_to_BC_Cow As Single
    Public N_to_feaces_Cow As Single
    Public N_to_urine_Cow As Double
    Public ReadOnly Property LWt_Change_Cow() As Single
        Get
            Return myMilkingHerd.LWt_Change
        End Get
    End Property

    Public Function PaddockCount() As Integer
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

    Public Property PorportionOfFarmInLaneWays() As Double
        Get
            Return myPorportionOfFarmInLaneWays
        End Get
        Set(ByVal value As Double)
            myPorportionOfFarmInLaneWays = value
        End Set
    End Property

    Public Property HoursOnLaneWays() As Double
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

    Public Property HoursInDairyShed() As Double
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
            myEffluentIrrigator.DebugLevel = value - 1
        End Set
    End Property

    Public Sub TestFeedWedge(ByVal post As Integer)
        SortPaddocksByCover()
        For Each pdk As LocalPaddockType In myPaddocks

        Next
        SortByIndex()
    End Sub

    'Amount of feed surplus/deficit [kgDM/ha]
    ' 1 = supply == demand i.e. in balance
    ' <1 = deficit
    ' >1 = surplus
    Public Function FeedSituation() As Double
        If (myMilkingHerd.Size() > 0) Then
            Dim post As Double = GrazingResidual
            Dim target As Double = IdealGrazingCover()
            Dim pre As Double = IdealPreGrazingCover() 'post + (target - post)
            Return AverageCover() - target
        Else
            Return 0
        End If
    End Function

    'Calculating the pre-grazing cover target - Method 1
    'Source: DairyNZ farmfact, 1-14 Pasture feed wedges [http://www.dairynz.co.nz/file/fileid/36306]
    'Parameters
    '       Stocking_Rate   [cows/ha]
    '       Intake          [kgDM/cow/day]
    '       Rotation        [days]
    '       Residual        [kgDM/ha]
    'Result [kgDM/ha]
    Private Function CalcPregrazingCoverTarget(ByVal Stocking_Rate As Double, ByVal Intake As Double, ByVal Rotation As Double, ByVal Residual As Double) As Double
        Dim calcPre As Double = (Stocking_Rate * Intake * Rotation) + Residual
        If (Stocking_Rate <= 0.1) Then
            calcPre = 4000
        End If
        Dim maxPre As Double = 10000 '3860
        Return Math.Min(calcPre, maxPre)
    End Function

    ' Source: DairyNZ - Feed Wedge Reconer
    Public Function IdealGrazingCover() As Double
        'Dim Cows_ha As Double = StockingRate
        'Dim KgDM_Cow As Double = myMilkingHerd.ME_Demand_Cow / 11.5 '17 'from doc
        'Dim RotationLength As Double = GrazingInterval
        'Dim TargetResidual As Double = GrazingResidual
        'Dim KgDM_ha As Double = Cows_ha * KgDM_Cow * RotationLength + TargetResidual
        'Return KgDM_ha
        Return (IdealPreGrazingCover() + myGrazingResidual) / 2.0
    End Function

    Public Function IdealPreGrazingCover() As Double
        'Dim post As Double = GrazingResidual
        'Dim average As Double = IdealGrazingCover()
        'Return average + (average - post)
        Return CalcPregrazingCoverTarget(MilkingCows() / FarmArea, myMilkingHerd.ME_Demand_Cow / DefaultPastureME, myGrazingInterval, myGrazingResidual)
    End Function

    Public Function PreGrazingCover() As Double
        Return 0 'need to store this somehow
        'myPreGraze = Max(myPreGraze, currentPdk.Cover)
        'onPrepare -> myPregraze = 0
    End Function

    Public Function GetEffluentArea() As Double
        Dim result As Double = 0
        For Each pdk As LocalPaddockType In myEffluentPaddocks
            result += pdk.Area
        Next
        Return result
    End Function

    'Fertiliser is not applied to eff paddocks
    'return the amount of fertiliser applied in kg/ha for the whole farm
    Public Function Fertilise(ByVal data As FertiliserApplicationType, ByVal ApplyToEffPdks As Boolean) As Double
        If (data.Amount > 0) Then
            Dim totalAmount As Double = 0
            Dim totalArea As Double = 0
            'If (Not ApplyToEffPdks) Then 'adjust amount applied to match LUDF reporting - removal because weekly reporting is based on non-effluent area
            '        Dim effArea As Double = GetEffluentArea()
            '        data.Amount *= MyFarmArea / (MyFarmArea - effArea)
            'End If
            For Each pdk As LocalPaddockType In myPaddocks
                If (myEffluentPaddocks Is Nothing Or ApplyToEffPdks) Then 'apply to all paddocks
                    pdk.Apply(data)
                    totalArea += pdk.Area
                    totalAmount += (data.Amount * pdk.Area)
                ElseIf Not (myEffluentPaddocks.Contains(pdk)) Then
                    pdk.Apply(data)
                    totalArea += pdk.Area
                    totalAmount += (data.Amount * pdk.Area)
                End If
            Next
            'Console.WriteLine("Fertilise {0:d} / {1:d} = {2:d}", total, FarmArea, total / FarmArea)
            If (DebugLevel > 0) Then
                Console.WriteLine("Fertilise " + totalAmount.ToString() + " / " + FarmArea.ToString() + " = " + (totalAmount / FarmArea).ToString())
            End If
            Return totalAmount / totalArea
        Else
            Return 0.0
        End If
    End Function

    Function Irrigate(ByVal data As IrrigationApplicationType, ByVal efficiency As Double) As Double
        If (data.Amount > 0) Then
            Dim total As Double = 0
            Dim area As Double = 0
            data.Amount *= efficiency
            For Each paddock As LocalPaddockType In myPaddocks
                area += data.Crop_Area
                total += (data.Amount * data.Crop_Area / efficiency)
                paddock.Irrigate(data)
            Next
            If (DebugLevel > 0) Then
                Console.WriteLine(total / FarmArea)
            End If
            Return total / area
        Else
            Return 0.0
        End If
    End Function

    Public ReadOnly Property PlantAvalibleWater(ByVal atDepth As Single) As Single
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
    Public Property EffluentPaddocksPercentage() As Double
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
        myMilkingHerd.setMilkSolids(values)
    End Sub

    Public Function getMilkSolids() As Double()        
        Return myMilkingHerd.getMilkSolids()
    End Function

    Public Sub setLiveWeight(ByVal values As Double())
        'Todo 20110524 - add checking here
        myMilkingHerd.setLiveWeight(values)
    End Sub

    Public Function getLiveWeight() As Double()
        Return myMilkingHerd.getLiveWeight()
    End Function


#Region "Effluent Retun from Dairy Shed"
    Dim myEffluentPaddocks As List(Of LocalPaddockType) = New List(Of LocalPaddockType)

    'Simulation passing a list of paddock names
    Sub setEffluentPaddocks(ByVal values As String())
        If (values Is Nothing) Then
            myEffluentPaddocks = New List(Of LocalPaddockType)()
            Return
        End If
        If (values.Length > 0) Then
            myEffluentPaddocks = New List(Of LocalPaddockType)(values.Length)
            For Each strPaddockName As String In values
                If (myPaddocks2.ContainsKey(strPaddockName)) Then
                    Dim p As LocalPaddockType = myPaddocks2(strPaddockName)
                    Console.WriteLine(p)
                    myEffluentPaddocks.Add(myPaddocks2(strPaddockName))
                End If
            Next
        Else
            myEffluentPaddocks = Nothing
        End If
    End Sub

    Function getEffluentPaddocks() As String()
        If (myEffluentPaddocks Is Nothing) Then
            Return New String() {""}
        End If
        Dim result(myEffluentPaddocks.Count) As String
        Dim i As Integer = 0
        For Each pdk As LocalPaddockType In myEffluentPaddocks
            result(i) = pdk.Name
            i += 1
        Next
        Return result
    End Function

    Sub doSprayEffluient()
        If (end_week And myEffluentPond.Volume > 0) Then
            Dim aList As New List(Of LocalPaddockType)
            If (myEffluentPaddocks IsNot Nothing) Then 'effluent paddock set via a list
                Console.Out.WriteLine("Spraying dairy shed effluent to paddocks;")
                For Each pdk As LocalPaddockType In myEffluentPaddocks
                    Console.Out.WriteLine(" - " + pdk.Name)
                Next
                myEffluentIrrigator.Irrigate(myEffluentPond, myEffluentPaddocks)
            Else
                Dim i As Integer = Math.Round(myEffluentPaddocksPercentage * myPaddocks.Count)
                aList = myPaddocks.GetRange(0, i)
                Console.Out.WriteLine("Spraying dairy shed effluent to " + i.ToString() + " paddocks")
                myEffluentIrrigator.Irrigate(myEffluentPond, aList)
            End If
        End If
    End Sub
#End Region

#Region "Laneways"
    Dim myLanewayPaddocks As List(Of LocalPaddockType) = New List(Of LocalPaddockType)
    'Simulation passing a list of paddock names
    Sub setLanewayPaddocks(ByVal values As String())
        myLanewayPaddocks = New List(Of LocalPaddockType)()
        If (values IsNot Nothing) AndAlso (values.Length > 0) Then
            For Each strPaddockName As String In values
                If (myPaddocks2.ContainsKey(strPaddockName)) Then
                    Dim p As LocalPaddockType = myPaddocks2(strPaddockName)
                    p.Grazable = False 'not part of grazing rotation
                    myLanewayPaddocks.Add(myPaddocks2(strPaddockName))
                    myPaddocks.Remove(p)
                End If
            Next
        End If
        If (myLanewayPaddocks.Count > 0) Then
            Dim newArea As Double = FarmArea / myPaddocks.Count
            For Each pdk As LocalPaddockType In myPaddocks
                pdk.Area = newArea
            Next
            Dim lArea As Double = FarmArea * myPorportionOfFarmInLaneWays / myLanewayPaddocks.Count
            For Each pdk As LocalPaddockType In myLanewayPaddocks
                pdk.Area = lArea
            Next
        End If
    End Sub
#End Region

    Public Sub PrintFarmSummary()
        Console.WriteLine("     General Farm Description")
        Console.WriteLine("             Effective Area          " & FarmArea.ToString("0.0") & " ha")
        Console.WriteLine("             Total Paddocks          " & PaddockCount())
        Console.WriteLine("     Stock Management")
        Console.WriteLine("             Stocking Rate           " & StockingRate & " cows/ha")
        Console.WriteLine("             Calving Date            ")
        Console.WriteLine("             Paddock Count           " & PaddockCount())
        Console.WriteLine("             Winter Off Dry Stock    " & WinterOffDryStock.ToString)
        Console.WriteLine("     Grazing Management")
        Console.WriteLine("             Residules               " & GrazingResidual & " kgDM/ha")
        Console.WriteLine("             Interval                " & GrazingInterval & " days")
        Console.WriteLine("     Supplementary Feeding")
        Console.WriteLine("             ME Content              " & SupplementME & " ME/kgDM")
        Console.WriteLine("             N Content               " & SupplementN.ToString("0.0%"))
        Console.WriteLine("             Wastage                 " & SupplementWastage.ToString("0.0%"))
        Console.WriteLine("     Conservation")
        Console.WriteLine("             Start Date              " & FCD.ToString("dd-MMM"))
        Console.WriteLine("             Finish Date             " & LCD.ToString("dd-MMM"))
        Console.WriteLine("             Trigger Residule        " & CDM & " kgDM/ha")
        Console.WriteLine("             Cutting Residule        " & CR & " kgDM/ha")
        Console.WriteLine("             Wastage at cutting      " & SilageCutWastage.ToString("0.0%"))
        Console.WriteLine("             Silage Stored on Farm   " & EnableSilageStore.ToString)
        If (EnableSilageStore) Then
            Console.WriteLine("             ME Content              " & SilageME.ToString("0.0") & " ME/kgDM")
            Console.WriteLine("             N Content               " & SilageN.ToString("0.0%"))
            Console.WriteLine("             Wastage (at feeding)    " & SilageWastage.ToString("0.0%"))
        End If
        If (DebugLevel > 0) Then

            Console.WriteLine("     Grazing Paddocks")
            For Each pdk As LocalPaddockType In myPaddocks
                Console.WriteLine("             " & pdk.ToString())
            Next
            Console.WriteLine("     Effluent Paddocks")
            For Each pdk As LocalPaddockType In myEffluentPaddocks
                Console.WriteLine("             " & pdk.ToString())
            Next
            Console.WriteLine("     Laneway Paddocks")
            For Each pdk As LocalPaddockType In myLanewayPaddocks
                Console.WriteLine("             " & pdk.ToString())
            Next
        End If
    End Sub

    Public Function TotalCows() As Double
        Return DryCows() + MilkingCows()
    End Function

    Public Function DryCows() As Double
        Return myDryCowHerd.Size()
    End Function

    Public Function MilkingCows() As Double
        Return myMilkingHerd.Size()
    End Function

    Property DryOffProportion As Double
        Get
            If (TotalCows() <= 0) Then
                Return 0
            End If
            Return DryCows() / TotalCows()
            '  Return myDryOffProportion
        End Get
        Set(ByVal value As Double)
            If (value < 0) Then
                value = 0
            ElseIf (value > 1) Then
                value = 1
            End If
            Dim total As Double = TotalCows()
            myDryCowHerd.setCowNumbers(total * value)
            myMilkingHerd.setCowNumbers(total * (1 - value))
        End Set
    End Property
End Class
