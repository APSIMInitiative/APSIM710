Public Class SimpleCow
        Inherits Instance
        ' The energy requirements in this clas are based on 
        '    DairyNZ farmfact 1-7 Feed requirements of milking cows (2006)
        '    DairyNZ farmfact 1-7 Feed requirements of dry cows (2006)

        Public Live_Weight As Double = 480
        Public CondistionScore As Double = 3
        Public MS_per_Day As Double = 0
        Public Change_in_KgLWt_per_Day As Double = 0
        Private Change_in_CondistionScore_per_Day As Double = 0
        Public Change_in_MS_per_Day As Double = 0
        'Claving Date & spread;
        'Drying off
        Private myCalvingDate As Integer = -1 '-1 == no calving date set
        Public Month_Of_Pregnancy = -1 '-1 == not pregnent
        Public Month_Of_Lactation = -1 '-1 == not lactating

        Dim Total_DM_Eaten, Total_Pasutre_Eaten, Total_Supplement_Eaten As BioMass

        Public TodaysEnergyRequirement As Double = 0
        Private RFD As Single = 0
        Public DoInterpolate As Boolean = True

        Public N_to_feaces, DM_to_feaces, N_to_urine, N_to_Milk, N_to_BC As Double

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

        Public ReadOnly Property N_Eaten_Supplement() As Double
                Get
                        Return Total_Supplement_Eaten.N_Total
                End Get
        End Property
#End Region
#End Region

        Public Property RemainingFeedDemand() As Double
                Get
                        Return RFD
                End Get
                Set(ByVal value As Double)
                        If (value < 0) Then
                                RFD = 0
                        Else
                                RFD = Math.Round(value * 100) / 100
                        End If
                End Set
        End Property

        Private Month As Integer
        'Private MilkCurve As Double() = {1.6, 1.49, 1.45, 1.3, 1.21, 0, 0.22, 1.56, 1.86, 1.95, 1.91, 1.67} 'kgMS/Day
        'Private LWtChange As Double() = {0, 0, 0, 0, -35.0, 0, 35.0, 0, 0, 0, 0, 0} 'kg LWt/Month
        'Month	1	2	3	4	5	6	7	8	9	10	11	12
        Private BC As Double() = {3.8, 3.9, 4.2, 4.4, 4.5, 5.1, 5, 5.1, 4, 3.8, 3.9, 3.9} 'dawn
        Private LWt As Double() = {437, 443, 450, 458, 466, 478, 486, 463, 430, 430, 438, 442} 'dawn
        Private MS As Double() = {1.5, 1.33, 1.23, 1.13, 1.12, 0, 0, 0.78, 1.69, 1.94, 1.86, 1.68} 'dawn

        '********** Constructor ******************
        Public Sub New(ByVal Year As Integer, ByVal Month As Integer)
                Live_Weight = LWt(Month - 1)
        End Sub
        '********** Constructor ******************

        '********** Hooks for Apsim Events ******************
        Public Sub OnPrepare(ByVal year As Integer, ByVal month As Integer)
                setDate(year, month)
                TodaysEnergyRequirement = ME_Total() 'calculate this early in case it's is needed outside
                RemainingFeedDemand = TodaysEnergyRequirement
                Total_DM_Eaten = New BioMass()
                Total_Pasutre_Eaten = New BioMass()
                Total_Supplement_Eaten = New BioMass()
        End Sub
        '********** Hooks for Apsim Events ******************


        Public Function ME_WeightChange()
                Return ME_Change_In_Liveweight(Change_in_CondistionScore_per_Day * 35)
        End Function

        Public Function ME_Walking()
                Return Walking(0) 'cows not walking at all
        End Function

        Public Function ME_Pregnancy()
                If (Month_Of_Pregnancy > 0) Then
                        Return Pregnancy(Month_Of_Pregnancy)
                Else
                        Return 0
                End If
        End Function

        Public Function ME_Lactation()
                Return ME_Milk_Production(MS_per_Day)
        End Function

        Public Function ME_Maintance()
                If (isDry) Then
                        Return 0.55 * Live_Weight ^ 0.75
                Else
                        Return 0.6 * Live_Weight ^ 0.75
                End If
        End Function

        Private Function ME_Milk_Production(ByVal kgMS As Double)
                Return 65 * kgMS
        End Function

        'Source:
        'DairyNZ farmFact 1-7 Feed requirement of milking cows
        'DairyNZ farmFact 1-8 Feed requirement of dry cows        
        Private Function ME_Change_In_Liveweight(ByVal kgLwt As Double)
                If (kgLwt > 0) Then
                        If (isDry) Then
                                Return 46 * kgLwt 'Gaining LWt while dry (68 @ 10ME/kgDM, 46 @ 11ME/kgDM
                        Else
                                Return 39 * kgLwt 'Gaining LWt while milking
                        End If
                Else
                        Return 32 * kgLwt 'loosing LWt
                End If
        End Function

        Private Function Walking(ByVal km As Double)
                Return 1 * km
        End Function

        Private Function Pregnancy(ByVal MonthOfPregnancy As Integer)
                Select Case (MonthOfPregnancy)
                        Case 6 : Return 5
                        Case 7 : Return 10
                        Case 8 : Return 20
                        Case 9 : Return 30
                End Select
                Return 0
        End Function

        'Energy requirements of a simple animal [MJME/Day]
        Public Function ME_Total()
                Return ME_Maintance() + ME_Lactation() + ME_Pregnancy() + ME_Walking() + ME_WeightChange()
        End Function

        Public Function Graze(ByVal GrazingPaddocks As List(Of LocalPaddockType), ByVal GrazingResidual As Double) As BioMass
                Dim dmRemoved As BioMass
                Dim i As Integer = 0
                While (RemainingFeedDemand > 0 And i < GrazingPaddocks.Count)
                        dmRemoved = GrazingPaddocks(i).Graze(RemainingFeedDemand, GrazingResidual)
                        If (dmRemoved.DM_Total > 0) Then
                                Feed(dmRemoved, True)
                                'doNutrientReturns(GrazingPaddocks(i), dmRemoved) ' this will be going on the wrong paddock, should be yesterday intake and include silage and grain
                        End If
                        i = i + 1
                End While
                Dim PH As Double = Total_DM_Eaten.getME_Total() 'PH == pasture harvested
                Return Total_DM_Eaten
        End Function

        Public Function Graze(ByVal GrazingPaddock As LocalPaddockType, ByVal GrazingResidual As Double) As BioMass

                Dim dmRemoved As BioMass = GrazingPaddock.Graze(RemainingFeedDemand, GrazingResidual)
                If (dmRemoved.DM_Total > 0) Then
                        Feed(dmRemoved, True)
                End If
                Return Total_DM_Eaten
        End Function

        Public Sub doNitrogenPartioning()
                Dim N_in As Double = Total_DM_Eaten.N_Total
                '1st Milk production
                'Dim casein As Double = MS_per_Day * 3.5 / (3.5 + 4.0) 'assume kgMS from milk @ 4% Fat + 3.5% Protien
                'N_to_Milk = casein * 0.1567 'TN content in casein of 15.67% source: Rouch et. el. (2008) - Determination of a nitrogen conversion factor for protein content in Cheddar cheese
                N_to_Milk = MS_per_Day * 0.07 'Dawn's rule, works out slightly lower (only 4% lower) than my version above

                '2nd Change in body weight
                N_to_BC = Change_in_KgLWt_per_Day * 0.02 ' Source: National Academies Press on ‘Air Emissions from Animal Feeding Operations: Current Knowledge, Future Needs (2003)’ 

                'finally Dung and Urine 40:60 split as per Dawn's documentation
                Dim N_Excreta = N_in - (N_to_Milk + N_to_BC)
                N_to_feaces = N_Excreta * 0.4
                N_to_urine = N_Excreta - N_to_feaces
                If (N_to_urine.ToString.Contains("NaN")) Then
                        Console.WriteLine("DDRules (debug) - " & "Urine: N not a number")
                End If

                Dim DM_out = Total_DM_Eaten.DM_Total * (1 - Total_DM_Eaten.digestibility) 'digestability = 70% grass, 55% hay, 80% grain, time = 18-24, 30-40 & 12-14 Farm Tech manual A-154
                DM_to_feaces = DM_out
        End Sub

        Public Function doNutrientReturns(ByVal pdk As LocalPaddockType, ByVal UrineOut_N As Double, ByVal DungNitrogen As Double, ByVal DungDM As Double, ByVal density As Double) 'ByVal paddock As PaddockHolder)
                pdk.UrineApplication(UrineOut_N, UrineOut_N / 0.08, density) 'urine N concentration of 8g/l
                pdk.DungApplication(DungNitrogen, DungDM)
                Return 0
        End Function

        Public Sub Feed(ByVal feed As BioMass, ByVal isPasture As Boolean)
                If (feed.DM_Total > 0) Then
                        Total_DM_Eaten = Total_DM_Eaten.Add(feed)
                        If (isPasture) Then
                                Total_Pasutre_Eaten = Total_Pasutre_Eaten.Add(feed)
                        Else
                                Total_Supplement_Eaten = Total_Supplement_Eaten.Add(feed)
                        End If
                        RemainingFeedDemand -= feed.getME_Total()
                End If
        End Sub

        Private Sub setDate(ByVal year As Integer, ByVal mth As Integer)
                If (mth <> Month) Then
                        Month = mth
                        DoDates(mth)
                        adjustLiveWeight(year, mth)
                        setMilkProduction(year, mth)
                Else
                        If (DoInterpolate) Then
                                Live_Weight += Change_in_KgLWt_per_Day
                                CondistionScore += Change_in_CondistionScore_per_Day
                                'MS_per_Day += Change_in_MS_per_Day
                        End If
                End If
        End Sub

        Private Sub adjustLiveWeight(ByVal year As Integer, ByVal mth As Integer)
                Dim days As Integer = Date.DaysInMonth(year, mth)
                Dim indexMth = (mth + 12 - 1) Mod 12
                Live_Weight = LWt(indexMth)
                CondistionScore = BC(indexMth)
                Dim indexMth2 = (mth + 12) Mod 12
                Dim deltaLWt As Double = LWt(indexMth2) - LWt(indexMth)
                Change_in_KgLWt_per_Day = deltaLWt / days
                Dim deltaBC As Double = BC(indexMth2) - BC(indexMth)
                Change_in_CondistionScore_per_Day = deltaBC / days
        End Sub

        Private Sub setMilkProduction(ByVal year As Integer, ByVal mth As Integer)
                Dim days As Integer = Date.DaysInMonth(year, mth)
                Dim indexMth = (mth + 12 - 1) Mod 12
                MS_per_Day = MS(indexMth)
                Dim indexMth2 = (mth + 12) Mod 12
                Dim deltaMS As Double = MS(indexMth2) - MS(indexMth)
                Change_in_MS_per_Day = deltaMS / days
        End Sub

        Public Property CalvingDate() As Integer
                Get
                        Return myCalvingDate
                End Get
                Set(ByVal value As Integer)
                        myCalvingDate = value
                End Set
        End Property

        Private Sub DoDates(ByVal month As Integer)
                Month_Of_Pregnancy = month - (CalvingDate - 9) - 1
                If (Month_Of_Pregnancy < 0) Then
                        Month_Of_Pregnancy += 12
                End If
                If Month_Of_Pregnancy > 9 Then
                        Month_Of_Pregnancy = -1
                End If

                Month_Of_Lactation = month - CalvingDate
                If (Month_Of_Lactation < 0) Then
                        Month_Of_Lactation += 12
                End If
        End Sub

        Public ReadOnly Property isDry() As Boolean
                Get
                        Return ME_Lactation() <= 0
                End Get
        End Property
End Class
