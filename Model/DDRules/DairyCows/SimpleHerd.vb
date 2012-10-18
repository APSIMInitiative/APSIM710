Public Class SimpleHerd
    Public Enum FeedType
        Pasture
        Silage
        Supplement
    End Enum

    Public ReferenceCow As SimpleCow
    Private TotalCows As Double = 0

    Dim Total_DM_Eaten As BioMass = New BioMass()
    Dim Total_Pasutre_Eaten As BioMass = New BioMass()
    Dim Total_Silage_Eaten As BioMass = New BioMass()
    Dim Total_Supplement_Eaten As BioMass = New BioMass()
    Public ME_Demand As Double

    Public myExcreta As New Excreta()
    Public myTempExcreta As New Excreta()

    Public Sub New()
        ReferenceCow = New SimpleCow(2001, 6)
        setValues(6, 0, 6)
    End Sub

    Public Sub New(ByVal Head As Double, ByVal CalvingDate As Integer, ByVal Year As Integer, ByVal Month As Integer)
        ReferenceCow = New SimpleCow(Year, Month)
        setValues(CalvingDate, Year, Month)
        TotalCows = Head
    End Sub

    Public Sub setValues(ByVal CalvingDate As Integer, ByVal Year As Integer, ByVal Month As Integer)
        ReferenceCow.setDate(Year, Month)
        ReferenceCow.CalvingDate = CalvingDate
        'TotalCows = Head
    End Sub

#Region "Output Variables - Intake"
#Region "Output Variables - Intake - Dry Matter"
    Public ReadOnly Property DM_Eaten() As Double
        Get
            Return Total_DM_Eaten.DM_Total
        End Get
    End Property
    Public ReadOnly Property DM_Eaten_Pasture() As Double
        Get
            Return Total_Pasutre_Eaten.DM_Total
        End Get
    End Property
    Public ReadOnly Property DM_Eaten_Silage() As Double
        Get
            Return Total_Silage_Eaten.DM_Total
        End Get
    End Property
    Public ReadOnly Property DM_Eaten_Supplement() As Double
        Get
            Return Total_Supplement_Eaten.DM_Total
        End Get
    End Property
#End Region
#Region "Output Variables - Intake - Energy"
    Public ReadOnly Property ME_Eaten() As Double
        Get
            Return Total_DM_Eaten.getME_Total
        End Get
    End Property
    Public ReadOnly Property ME_Eaten_Pasture() As Double
        Get
            Return Total_Pasutre_Eaten.getME_Total
        End Get
    End Property
    Public ReadOnly Property ME_Eaten_Silage() As Double
        Get
            Return Total_Silage_Eaten.getME_Total
        End Get
    End Property
    Public ReadOnly Property ME_Eaten_Supplement() As Double
        Get
            Return Total_Supplement_Eaten.getME_Total
        End Get
    End Property
#End Region
#Region "Output Variables - Intake - Nitrogen"
    Public ReadOnly Property N_Eaten() As Double
        Get
            Return Total_DM_Eaten.N_Total
        End Get
    End Property
    Public ReadOnly Property N_Eaten_Pasture() As Double
        Get
            Return Total_Pasutre_Eaten.N_Total
        End Get
    End Property
    Public ReadOnly Property N_Eaten_Silage() As Double
        Get
            Return Total_Silage_Eaten.N_Total
        End Get
    End Property
    Public ReadOnly Property N_Eaten_Supplement() As Double
        Get
            Return Total_Supplement_Eaten.N_Total
        End Get
    End Property
#End Region
#End Region
#Region "Output Variables - Outputs"
#Region "Output Variables - Outputs - Nitrogen/Carbon"
    Public ReadOnly Property N_to_feaces() As Double
        Get
            Return ReferenceCow.N_to_feaces * TotalCows
        End Get
    End Property
    Public ReadOnly Property DM_to_feaces() As Double
        Get
            Return ReferenceCow.DM_to_feaces * TotalCows
        End Get
    End Property
    Public ReadOnly Property N_to_urine() As Double
        Get
            Return ReferenceCow.N_to_urine * TotalCows
        End Get
    End Property
    Public ReadOnly Property N_to_Milk() As Double
        Get
            Return ReferenceCow.N_to_Milk * TotalCows
        End Get
    End Property
    Public ReadOnly Property N_to_BC() As Double
        Get
            Return ReferenceCow.N_to_BC * TotalCows
        End Get
    End Property
    Public ReadOnly Property N_Balance() As Double
        Get
            Return N_Eaten - N_Out
        End Get
    End Property
    Public ReadOnly Property N_Out() As Double
        Get
            Return N_to_Milk + N_to_feaces + N_to_urine + N_to_BC
        End Get
    End Property
#End Region
#End Region

    Public Function RemainingFeedDemand() As Double
        Return ReferenceCow.RemainingFeedDemand * TotalCows
    End Function

    Public Function TodaysEnergyRequirement() As Double
        Return ReferenceCow.TodaysEnergyRequirement * TotalCows
    End Function

    Public Function isDry() As Boolean
        Return ReferenceCow.isDry
    End Function

    Public Sub onPrepare(ByVal Year As Integer, ByVal Month As Integer)
        ReferenceCow.OnPrepare(Year, Month)
        Total_DM_Eaten = New BioMass()
        Total_Pasutre_Eaten = New BioMass()
        Total_Silage_Eaten = New BioMass()
        Total_Supplement_Eaten = New BioMass()
        'setCowNumbers(numCows)
    End Sub

    Public ReadOnly Property ME_Maintance() As Double
        Get
            Return ReferenceCow.ME_Maintance * TotalCows
        End Get
    End Property

    Public ReadOnly Property ME_Maintance_Cow() As Double
        Get
            Return ReferenceCow.ME_Maintance
        End Get
    End Property

    Public ReadOnly Property ME_WeightChange() As Double
        Get
            Return ReferenceCow.ME_WeightChange * TotalCows
        End Get
    End Property

    Public ReadOnly Property ME_WeightChange_Cow() As Double
        Get
            Return ReferenceCow.ME_WeightChange
        End Get
    End Property

    Public ReadOnly Property ME_Lactation() As Double
        Get
            Return ReferenceCow.ME_Lactation * TotalCows
        End Get
    End Property

    Public ReadOnly Property ME_Lactation_Cow() As Double
        Get
            Return ReferenceCow.ME_Lactation
        End Get
    End Property

    Public ReadOnly Property ME_Pregnancy() As Double
        Get
            Return ReferenceCow.ME_Pregnancy * TotalCows
        End Get
    End Property

    Public ReadOnly Property ME_Pregnancy_Cow() As Double
        Get
            Return ReferenceCow.ME_Pregnancy
        End Get
    End Property

    Public ReadOnly Property ME_Walking() As Double
        Get
            Return ReferenceCow.ME_Walking * TotalCows
        End Get
    End Property

    Public ReadOnly Property ME_Walking_Cow() As Double
        Get
            Return ReferenceCow.ME_Walking
        End Get
    End Property
    Public ReadOnly Property ME_Total() As Double
        Get
            Return ReferenceCow.ME_Total * TotalCows
        End Get
    End Property

    Public Function ME_Total_Cow() As Double
        Return ReferenceCow.ME_Total
    End Function

    Public Sub setCowNumbers(ByVal numCows As Double)
        TotalCows = numCows
        ME_Demand = ReferenceCow.TodaysEnergyRequirement * TotalCows
    End Sub

    Public Sub Feed(ByVal feed As BioMass, ByVal type As FeedType)
        If (feed.DM_Total > 0) Then
            Select Case type
                Case FeedType.Pasture
                    Total_Pasutre_Eaten = Total_Pasutre_Eaten.Add(feed)
                Case FeedType.Silage
                    Total_Silage_Eaten = Total_Silage_Eaten.Add(feed)
                Case FeedType.Supplement
                    Total_Supplement_Eaten = Total_Supplement_Eaten.Add(feed)
            End Select

            'If (isPasture) Then
            '        Total_Pasutre_Eaten = Total_Pasutre_Eaten.Add(feed)
            'Else
            '        Total_Supplement_Eaten = Total_Supplement_Eaten.Add(feed)
            'End If
            Total_DM_Eaten = Total_DM_Eaten.Add(feed)
            ReferenceCow.Feed(feed.Multiply(1 / TotalCows), type = FeedType.Pasture)
        End If
    End Sub

    Public Function Graze(ByVal GrazingPaddock As PaddockWrapper, ByVal GrazingResidual As Double) As BioMass
        Dim dmRemoved As BioMass = GrazingPaddock.Graze(RemainingFeedDemand, GrazingResidual)
        If (dmRemoved.DM_Total > 0) Then
            Feed(dmRemoved, FeedType.Pasture)
        End If
        Return Total_DM_Eaten
    End Function

    Public Function isUnderFed() As Boolean
        Return RemainingFeedDemand() > 0
    End Function

    Public Function Live_Weight() As Double
        Return ReferenceCow.Live_Weight
    End Function

    Public Function LWt_Change() As Double
        Return ReferenceCow.Change_in_KgLWt_per_Day
    End Function

    Public Function BC() As Double
        Return ReferenceCow.ConditionScore
    End Function
    Private Function MyCStr(ByVal d As Double) As String
        Return CStr(d)
    End Function
    Public Function Cow_BC_ByMonth_() As String
        Dim b() As String = Array.ConvertAll(Of Double, String)(ReferenceCow.BC_ByMonth, New Converter(Of Double, String)(AddressOf MyCStr))
        Dim c As String = String.Join(",", b)
        Return c
    End Function

    Public Function Month_Of_Pregnancy() As Integer
        Return ReferenceCow.Month_Of_Pregnancy
    End Function
    Public Function Month_Of_Lactation() As Integer
        Return ReferenceCow.Month_Of_Lactation
    End Function

    Public Function MS_per_Day() As Single
        Return ReferenceCow.MS_per_Day * TotalCows
    End Function

    Public Function MS_per_Day_Cow() As Single
        Return ReferenceCow.MS_per_Day
    End Function

    Public Sub doNitrogenPartioning()
        ReferenceCow.doNitrogenPartioning()
        myExcreta = ReferenceCow.myExcreta.Multiply(TotalCows)
        myTempExcreta = New Excreta(myExcreta) ' this for returning
    End Sub

    Public Function getNutrientReturns(ByVal proporiton As Double) As Excreta
        Dim amount As Excreta = myExcreta.Multiply(proporiton)
        myTempExcreta.Subtract(amount)
        Return amount
    End Function

    'Return all nutrients evenly to the paddocks in the list
    ' Todo: distribute nutrients by amount of drymatter removal (if grazed)
    Public Sub doNutrientReturns(ByVal myGrazedPaddocks As List(Of PaddockWrapper))
        Dim urineN As Double = N_to_urine
        Dim dungN As Double = N_to_feaces
        Dim dungDM As Double = DM_to_feaces

        Dim totalMERemoved As Double = 0
        For Each pdk As PaddockWrapper In myGrazedPaddocks
            totalMERemoved += pdk.ME_Eaten
        Next

        If (TotalCows > 0 And myGrazedPaddocks.Count > 0) Then
            If totalMERemoved <= 0 Then 'if no grazing then distribute evenly over all paddocks (should be only grazable)
                Dim delta As Double = 1 / myGrazedPaddocks.Count
                Dim density As Double = delta * TotalCows
                For Each pdk As PaddockWrapper In myGrazedPaddocks
                    ReferenceCow.doNutrientReturns(pdk, urineN * delta, dungN * delta, dungDM * delta, density)
                Next
            Else 'if grazed then distribute over paddocks by amount of ME removed
                For Each pdk As PaddockWrapper In myGrazedPaddocks
                    Dim delta As Double = pdk.ME_Eaten / totalMERemoved
                    Dim density As Double = delta * TotalCows / pdk.Area
                    ReferenceCow.doNutrientReturns(pdk, urineN * delta, dungN * delta, dungDM * delta, density)
                Next
            End If
        End If
    End Sub

    Public Sub doNutrientReturnsToPaddock(ByVal paddock As List(Of PaddockWrapper), Optional ByVal proporiton As Double = 1.0)
        Dim area As Double = 0
        For Each pdk As PaddockWrapper In paddock
            area += pdk.Area
        Next
        If (area <= 0) Then
            Return 'i.e. nutrient is removed from the system
            'Todo 20110608 - Store and return this value for nitrogen balance calculations
        End If
        Dim amountTotal As Excreta = getNutrientReturns(proporiton)
        Dim amountHa As Excreta = amountTotal.Multiply(1 / area)
        For Each pdk As PaddockWrapper In paddock
            Dim amount As Excreta = amountHa.Multiply(pdk.Area)
            ReferenceCow.doNutrientReturns(pdk, amount.N_to_urine, amount.N_to_feaces, amount.DM_to_feaces, TotalCows / pdk.Area * proporiton)
        Next
    End Sub

    Public Sub doNutrientReturnsToPaddock(ByVal paddock As PaddockWrapper, ByVal amount As Excreta, Optional ByVal proporiton As Double = 1.0)
        ReferenceCow.doNutrientReturns(paddock, amount.N_to_urine, amount.N_to_feaces, amount.DM_to_feaces, TotalCows / paddock.Area * proporiton)
    End Sub

    '       Public Sub doNutrientReturns(ByVal myPaddocks As List(Of PaddockWrapper), Optional ByVal proporiton As Double = 1.0)

    '      End Sub

    Public Function ME_Demand_Cow() As Double
        Return ReferenceCow.ME_Total
    End Function
    Public Function ME_Eaten_Cow() As Double
        Return ReferenceCow.ME_Eaten
    End Function
    Public Function ME_Eaten_Pasture_Cow() As Double
        Return ReferenceCow.ME_Eaten_Pasture
    End Function
    Public Function ME_Eaten_Supplement_Cow() As Double
        Return ReferenceCow.ME_Eaten_Supplement
    End Function
    Public Function DM_Eaten_Cow() As Double
        Return ReferenceCow.DM_Eaten
    End Function
    Public Function DM_Eaten_Pasture_Cow() As Double
        Return ReferenceCow.DM_Eaten_Pasture
    End Function
    Public Function DM_Eaten_Supplement_Cow() As Double
        Return ReferenceCow.DM_Eaten_Supplement
    End Function
    Public Function N_Eaten_Cow() As Double
        Return ReferenceCow.N_Eaten
    End Function
    Public Function N_Eaten_Pasture_Cow() As Double
        Return ReferenceCow.N_Eaten_Pasture
    End Function
    Public Function N_Eaten_Supplement_Cow() As Double
        Return ReferenceCow.N_Eaten_Supplement
    End Function
    Public Function N_to_milk_Cow() As Double
        Return ReferenceCow.N_to_Milk
    End Function
    Public Function N_to_BC_Cow() As Double
        Return ReferenceCow.N_to_BC
    End Function
    Public Function N_to_feaces_Cow() As Double
        Return ReferenceCow.N_to_feaces
    End Function
    Public Function N_to_urine_Cow() As Double
        Return ReferenceCow.N_to_urine
    End Function
    Public Sub setCow_BC(ByVal values As Double())
        ReferenceCow.setCow_BC(values)
    End Sub
    Public Sub setMilkSolids(ByVal values As Double())
        'Todo 20110524 - add checking here
        ReferenceCow.setMilkSolids(values)
    End Sub
    Public Function getMilkSolids() As String
        Return ReferenceCow.getMilkSolids()
    End Function
    Public Sub setLiveWeight(ByVal values As Double())
        'Todo 20110524 - add checking here
        ReferenceCow.setLiveWeight(values)
    End Sub
    Public Function getLiveWeight() As String
        Return ReferenceCow.getLiveWeight()
    End Function

    Public Function Size() As Double
        Return TotalCows
    End Function
End Class
