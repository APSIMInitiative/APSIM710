Imports ModelFramework

Public Class Farm
   Inherits Instance
   Private myDebugLevel As Integer = 0
   Private myPaddocks As List(Of LocalPaddockType)         ' Full list of apsim paddocks
   Private myHerd As SimpleHerd                            ' Dairy herd / on milking platform
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
   Private end_week As Integer

   Public Sub New()
      myHerd = New SimpleHerd(1, 6, 0, 6)
   End Sub

   Public Sub Init(ByVal MasterPM As Paddock, ByVal Year As Integer, ByVal Month As Integer, ByVal FarmArea As Double)
      myPaddocks = New List(Of LocalPaddockType)
      PaddockQueue = New Queue(Of LocalPaddockType)
      GrazedList = New List(Of LocalPaddockType)
      Dim i As Integer = -1
      ' find paddocks with an area property set by user
      ' Loop throught all paddocks
      ' find paddock with an area set
      ' add up the total area set by user
      ' store paddock and area for latter use
      Dim SpecifiedArea = 0
      Dim TempList As New Dictionary(Of Paddock, Double)
      'How do we get the area of indervidual paddocks?
      For Each SubPaddock As Paddock In MasterPM.SubPaddocks
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
      Next

      MyFarmArea = FarmArea
      Dim UnallocatedArea = MyFarmArea - SpecifiedArea
      If (UnallocatedArea < 0) Then ' more area set manually per paddock than set by the farm component
         'throw a fatal error'
      End If

      Dim DefaultArea As Double = 0
      Dim UnallocatedPaddocks = MasterPM.SubPaddocks.Count - TempList.Count
      If (UnallocatedPaddocks > 0) Then ' if zero then all paddock have a area attached to them. No calculations need to be done
         DefaultArea = UnallocatedArea / UnallocatedPaddocks
      End If

      For Each SubPaddock As Paddock In MasterPM.SubPaddocks
         i += 1
         Dim TempArea = DefaultArea
         If (TempList.ContainsKey(SubPaddock)) Then
            TempArea = TempList.Item(SubPaddock)
         End If

         'TempArea = 1
         myPaddocks.Add(New LocalPaddockType(i, SubPaddock, TempArea))
         If (myDebugLevel > 0) Then
            Console.WriteLine("   Paddock " & myPaddocks(i).ToString)
         End If
      Next
      'myHerd = New SimpleHerd(1, 6, Year, Month) ' moved to constructor
      SilageHeap = New FeedStore
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

   Public Sub Process()
      updateCovers()
      CheckWinteringOff()
      If Not (IsWinteringOff()) Then 'assume all stock wintering off farm i.e. no grazing
         If (PaddockQueue.Count = 0) Then 'either it is time to shift or have completed a full rotation
            Allocate_Paddocks()
         End If
         Graze()
         doAnimalsPost()
      End If
      doConservation()
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
         If (p.GrazingCounter <= 0) Then 'p.AvalibleDryMater <= 1 Or 
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

   <Units("0-1")> Public SupplementDigestability As Double = 0.8
   <Units("0-1")> Public SilageDigestability As Double = 0.68 'need to check this value
   <Units("kgN/kgDM")> Public SilageN As Double = 0.035 'N content of silage (need to chechk this value - add to the user interface
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
         FeedSilage(myHerd.RemainingFeedDemand, SilageWastage)
      End If

      If (myHerd.RemainingFeedDemand > 0) Then ' Meet any remaining demand with bought in feed (i.e. grain)
         FeedSupplement(myHerd.RemainingFeedDemand, SupplementME, SupplementN, SupplementWastage, SupplementDigestability)
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

   Function FeedSilage(ByVal MEDemand As Single, ByVal WastageFactor As Single) As Boolean
      Dim tempDM As BioMass = SilageHeap.Remove(MEDemand * (1 + WastageFactor))
      If (tempDM.DM_Total <= 0) Then
         Return False
      End If
      tempDM = tempDM.Multiply(1 - WastageFactor)
      tempDM.digestibility = SilageDigestability
      Dim tempEnergyDiff As Double = (tempDM.getME_Total - MEDemand) / MEDemand
      myHerd.Feed(tempDM, SimpleHerd.FeedType.Silage)
      Return tempDM.getME_Total > 0
   End Function

   Function FeedSupplement(ByVal MEDemand As Single, ByVal MEperKg As Single, ByVal NperKg As Single, ByVal WastageFactor As Single, ByVal Digestability As Single) As Boolean
      Dim dm As BioMass = New BioMass()
      Dim SupplementFedout As Double = (MEDemand / MEperKg) * (1 + WastageFactor)
      dm.gLeaf = SupplementFedout * (1 - WastageFactor)
      dm.setME(SupplementME)
      dm.digestibility = Digestability
      dm.N_Conc = NperKg
      myHerd.Feed(dm, SimpleHerd.FeedType.Supplement)
      SupplementStore.Add(dm)
      Return (dm.DM_Total > 0)
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

   Sub Allocate_Paddocks()
      SortPaddocksByCover()
      PaddockQueue = New Queue(Of LocalPaddockType)
      For Each Paddock As LocalPaddockType In myPaddocks
         If (Not Paddock.Closed And Paddock.Grazable) Then
            Paddock.GrazingCounter = Paddock.Area * myDayPerHa
            PaddockQueue.Enqueue(Paddock) 'add all paddock to the queue (including close ones)
         End If
      Next
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
            myDayPerHa = myGrazingInterval / FarmArea
            Allocate_Paddocks()
         End If
      End Set
   End Property

   Sub SortPaddocksByCover()
      'shufflePaddocks()
      myPaddocks.Sort(LocalPaddockType.getSortListByCover())
   End Sub

   Sub SortByIndex()
      myPaddocks.Sort(LocalPaddockType.getSortListByIndex())
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
      For Each Paddock As LocalPaddockType In myPaddocks
         TotalCover += Paddock.Cover
      Next
      Return TotalCover / FarmArea
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

   <Units("kgDM/ha")> Public CDM As Double = 3500 ' Conservation trigger pasture mass (Dawn default = 3500)
   <Units("kgDM/ha")> Public CR As Integer = 1600 ' Conservation cutting residual pasture mass (Dawn default)
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
      If isBetween(myDate, FCD, LCD) Then
         ClosePaddocks()
      End If

      Dim IsCuttingDay As Boolean = end_week > 0 'only cut once a week [as per Dawn's rules]
      If (IsCuttingDay And PaddocksClosed) Then
         Dim HarvestedDM As BioMass = doHarvest(SilageCutWastage)
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
         If Not (Paddock.Closed) And Paddock.Grazable And (Paddock.Cover > CDM) And Not (GrazedList.Contains(Paddock)) Then
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
End Class
