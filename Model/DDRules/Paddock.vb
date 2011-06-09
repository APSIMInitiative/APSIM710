Imports ModelFramework

Public Class LocalPaddockType
        Inherits Instance
        'Local paddock is the wrapper for the basic apsim paddock. It adds "worker" functions for processes such as
        ' nutrient grazing and returns.

        Private Enum PaddockStatus
                GR = 1  'Growing
                BG = 2 'Being grazed
                JG = 4 'Just grazed
                CL = 8  'Closed
                NA = 19  'Not avalible for grazing
        End Enum

        Public DebugLevel As Integer = 0 '0==none, 1==brief, 2==verbose
        Public Shared DebugTestBreakFeeding As Boolean = True
        Public Shared MovingAverageSeriesLength As Integer = 30

        <Link()> Private ApSim_SubPaddock As Paddock
        <Input(True)> Public Area As Double = -1

        Private myStatus As PaddockStatus
        'Homogeneous pasture cover
        Dim Pasture_Cover As BioMass = New BioMass()
        'Lookup for indervidual pasture species
        Public PastureMasses As Dictionary(Of String, BioMass) = New Dictionary(Of String, BioMass)
        Public myAverageGrowthRate As MovingAverage

        Private Default_N_Conc = 0.035
        Private Default_Digestability = 0.68
        Public index As Integer
        Public GrazingResidual As Double = 0
        Dim DM_Grazed As BioMass = New BioMass()
        Dim N_Feaces, C_Feaces, N_Urine As Double
        Public GrazingCounter As Integer = 0 'number of days this paddocked will be grazed for given the current rotation length
        Dim Default_Application_Depth As Double = 300 'mm

        Private UrinePatchComponent As Component
        Private AgPasture As Component

        Public Sub New(ByVal index As Integer, ByRef paddock As Paddock, ByVal PaddockArea As Double)
                ApSim_SubPaddock = paddock                              'store a local pointer to the ApSim "Subpaddock" 
                Me.Name = ApSim_SubPaddock.Name

                Area = PaddockArea
                Dim i As Integer = PaddockArea
                Dim j As Integer = Area

                Grazable = True                               'paddock grazable at initilisation time
                Me.index = index                                        'sort origional paddock position in the simulation for sorting
                UrinePatchComponent = ApSim_SubPaddock.ComponentByName("UrinePatch")
                AgPasture = ApSim_SubPaddock.ComponentByType("AgPasture")
                myAverageGrowthRate = New MovingAverage(MovingAverageSeriesLength)
        End Sub

        Public Sub New(ByVal index As Integer, ByRef paddock As Paddock)
                Me.New(index, paddock, 1.0)
        End Sub

        Sub OnPrepare()
                Dim i As Double = Area
                'If (i > 0) Then
                '        Console.WriteLine("AREA IS SET ******************************************************* " + ApSim_SubPaddock.Name + " " + i.ToString)
                'End If
                N_Feaces = 0
                C_Feaces = 0
                N_Urine = 0
                DM_Grazed = New BioMass()
                If (myStatus = PaddockStatus.JG) Then
                        Grazable = True
                End If
        End Sub

        Public Sub setJustGrazed()
                myStatus = PaddockStatus.JG
        End Sub

        Public Property BeingGrazed() As Boolean
                Get
                        Return myStatus = PaddockStatus.BG
                End Get
                Set(ByVal value As Boolean)
                        If (value) Then
                                myStatus = PaddockStatus.BG
                        Else
                                Select Case (BeingGrazed)
                                        Case PaddockStatus.BG
                                                myStatus = PaddockStatus.JG
                                        Case Else
                                                myStatus = PaddockStatus.GR
                                End Select
                        End If
                End Set
        End Property

        Public Property Closed() As Boolean
                Get
                        Return myStatus = PaddockStatus.CL
                End Get
                Set(ByVal value As Boolean)
                        If (value) Then
                                myStatus = PaddockStatus.CL
                        Else
                                Grazable = True
                        End If
                End Set
        End Property

        Public Property Grazable() As Boolean
                Get
                        Return myStatus <> PaddockStatus.NA
                End Get
                Set(ByVal value As Boolean)
                        If (value) Then
                                myStatus = PaddockStatus.GR
                        Else
                                myStatus = PaddockStatus.NA
                        End If
                End Set
        End Property

        Function Graze(ByVal energyRequired As Double, ByVal GrazingResidual As Double) As BioMass
                GrazingCounter -= 1
                If (DebugTestBreakFeeding) Then
                        Return GrazeBreak(energyRequired, GrazingResidual)
                Else
                        Return GrazeOld(energyRequired, GrazingResidual)
                End If
        End Function

        Function GrazeOld(ByVal energyRequired As Double, ByVal GrazingResidual As Double) As BioMass
                Me.GrazingResidual = GrazingResidual
                UpdateCovers()

                Dim PreGrazeMass As New BioMass(Pasture_Cover)
                If (DebugLevel > 0) Then
                        Console.WriteLine()
                        Console.WriteLine("DDRules (debug) - " & "  + Cover          = " & Cover())
                        Console.WriteLine("DDRules (debug) - " & "  - Residual       = " & GrazingResidual)
                        Console.WriteLine("DDRules (debug) - " & "  = Avalible DM    = " & AvalibleDryMater())
                        Console.WriteLine("DDRules (debug) - " & "  *          ME    = " & PastureME())
                        Console.WriteLine("DDRules (debug) - " & "  = Avalible ME    = " & AvalibleME())
                        Console.WriteLine("DDRules (debug) - " & "  - energyRequired = " & energyRequired)
                End If

                Dim RemovedME As Double
                Dim testDM1, testDM2, testDM3 As Double
                If (energyRequired < AvalibleME()) Then 'this paddock contains more ME that required...
                        'assue linear pasture removal/quality
                        If (DebugTestBreakFeeding) Then
                                Dim avgGR As Double = myAverageGrowthRate.Average
                                Dim predictedGrowth = avgGR * GrazingCounter
                                testDM1 = AvalibleME() + predictedGrowth * PastureME()
                                testDM2 = Math.Min(testDM1 / GrazingCounter, energyRequired)
                                testDM3 = GrazingResidual + ((AvalibleME() - testDM2) / AvalibleME()) * (Cover() - GrazingResidual) 'only graze down to the required residual

                                Dim testDM4 As Double = (Cover() - testDM3) * PastureME()
                                GrazingResidual = testDM3
                                RemovedME = testDM2
                                Console.WriteLine("DDRules (DebugTestBreakFeeding) - " & ", " & index & ", " & avgGR & ", " & GrazingCounter & ", " & predictedGrowth & ", " & testDM2 & ", " & energyRequired & ", " & testDM3 & ", " & Cover() & ", " & GrazingResidual)
                        Else
                                GrazingResidual += ((AvalibleME() - energyRequired) / AvalibleME()) * (Cover() - GrazingResidual) 'only graze down to the required residual
                                RemovedME = energyRequired      'harvest the required amount
                        End If
                Else
                        RemovedME = AvalibleME()          'harvest all avalible DM/ME
                End If
                BeingGrazed = True      ' might need to come back

                If (DebugLevel > 0) Then
                        Console.WriteLine("DDRules (debug) - " & "  = Grazing Residual = " & GrazingResidual)
                End If

                Dim result As BioMass = New BioMass()
                If (GrazingResidual < Cover()) Then
                        Dim MassRemoved As BioMass
                        For Each crop As Component In ApSim_SubPaddock.Crops
                                Dim tempBioMass As BioMass = New BioMass
                                PastureMasses.TryGetValue(crop.Name, tempBioMass) 'this should really be checked, it should never fail but...
                                Dim proportion As Double = tempBioMass.getME_Total / PreGrazeMass.getME_Total
                                If (ApSim_SubPaddock.Crops.Count = 1 And Math.Abs(proportion - 1.0) > 0.0000001) Then
                                        Console.WriteLine("DDRules Error - Grazing proportions not calculated correctly")
                                End If

                                MassRemoved = GrazePlant(crop, proportion * RemovedME / Area) 'bugger this is not going to work correctly with multiple plants (need to remove by proportion)
                                result = result.Add(MassRemoved.Multiply(Area))
                        Next
                End If

                UpdateCovers()
                If (DebugLevel > 0) Then
                        Dim PostGrazeMass As New BioMass(Pasture_Cover)
                        Dim Removal As BioMass = PreGrazeMass.Subtract(PostGrazeMass)
                        Dim Test As BioMass = PostGrazeMass.Add(Removal)
                        Console.WriteLine("DDRules (debug) - " & " Pre-Graze  = " & PreGrazeMass.ToString)
                        Console.WriteLine("DDRules (debug) - " & " Post-Graze = " & PostGrazeMass.ToString)
                        Console.WriteLine("DDRules (debug) - " & " Removal    = " & Removal.ToString)
                        Console.WriteLine("DDRules (debug) - " & " Test       = " & Test.ToString)

                        Dim removed2 = result.DM_Total
                        Console.WriteLine("DDRules (debug) - " & "  N Removed   = " & Removal.N_Total)
                        Console.WriteLine("DDRules (debug) - " & "  N Concentration   = " & Removal.N_Conc)
                        Console.WriteLine("DDRules (debug) - " & "  ME   = " & result.getME())
                        Console.WriteLine("DDRules (debug) - " & "       = " & Removal.getME)
                        Console.WriteLine("DDRules (debug) - " & "  + Pasture Removed   = " & removed2)
                        Console.WriteLine("DDRules (debug) - " & "                      = " & Removal.DM_Total)
                        Console.WriteLine("DDRules (debug) - " & "  = Energy Removed    = " & result.getME_Total)
                        Console.WriteLine("DDRules (debug) - " & "                      = " & Removal.getME_Total)
                End If
                DM_Grazed = result
                Return result
        End Function

        Function GrazeBreak(ByVal energyRequired As Double, ByVal GR As Double) As BioMass
                Me.GrazingResidual = GR
                UpdateCovers()
                Dim PreGrazeMass As New BioMass(Pasture_Cover)

                'Dim RemovedME As Double
                'Dim totalME_plusGR, ME_to_remove, New_Residual As Double
                Dim todayGR As Double
                Dim todayME As Double
                'If (energyRequired < AvalibleME()) Then 'this paddock contains more ME that required...
                Dim predictedME As Double = myAverageGrowthRate.Average * GrazingCounter * PastureME()
                Dim totalME As Double = AvalibleME() + predictedME
                todayME = totalME / GrazingCounter
                todayME = Math.Min(todayME, energyRequired)
                todayME = Math.Min(todayME, AvalibleME)
                todayME = Math.Max(todayME, 0)

                todayGR = GrazingResidual + todayME / PastureME()

                'totalME_plusGR = AvalibleME() + predictedGrowth * PastureME()
                'ME_to_remove = Math.Min(totalME_plusGR / GrazingCounter, energyRequired)
                'New_Residual = GR + ((AvalibleME() - ME_to_remove) / AvalibleME()) * (Cover() - GR) 'only graze down to the required residual

                'Dim testDM4 As Double = (Cover() - New_Residual) * PastureME()
                'GR = New_Residual
                'RemovedME = ME_to_remove
                'Console.WriteLine("DDRules (DebugTestBreakFeeding) - " & ", " & index & ", " & Cover() & ", " & GrazingCounter & ", " & predictedGrowth & ", " & ME_to_remove & ", " & energyRequired & ", " & New_Residual & ", " & Cover() & ", " & GR)
                'Else
                'RemovedME = AvalibleME()          'harvest all avalible DM/ME
                'todayGR = GrazingResidual
                'End If

                BeingGrazed = True      ' might need to come back

                Dim result As BioMass = New BioMass()
                'todayME = (Cover() - todayGR) * PastureME()
                '                If (todayGR < Cover()) Then
                Dim MassRemoved As BioMass
                For Each crop As Component In ApSim_SubPaddock.Crops
                        Dim tempBioMass As BioMass = New BioMass
                        PastureMasses.TryGetValue(crop.Name, tempBioMass) 'this should really be checked, it should never fail but...
                        Dim proportion As Double = tempBioMass.getME_Total / PreGrazeMass.getME_Total
                        If (ApSim_SubPaddock.Crops.Count = 1 And Math.Abs(proportion - 1.0) > 0.0000001) Then
                                Console.WriteLine("DDRules Error - Grazing proportions not calculated correctly")
                        End If

                        MassRemoved = GrazePlant(crop, proportion * todayME / Area) 'bugger this is not going to work correctly with multiple plants (need to remove by proportion)
                        If (MassRemoved.N_Total <= 0) Then
                                Console.WriteLine("DDRulues ERROR - What the....")
                        End If

                        result = result.Add(MassRemoved.Multiply(Area))
                Next
                '                End If

                UpdateCovers()
                DM_Grazed = result
                Return result
        End Function

        Function GrazePlant(ByVal crop As Component, ByVal RemovedME As Double) As BioMass
                Dim result As BioMass = New BioMass
                PastureMasses.TryGetValue(crop.Name, result) 'this should really be checked, but it should never fail :)
                Dim percentageRemoval As Double = RemovedME / result.getME_Total
                result = result.Multiply(percentageRemoval)
                crop.Publish("remove_crop_biomass", result.toRemoveCropDmType)

                If Not (AgPasture Is Nothing) Then
                        result.N_Conc = crop.Variable("AboveGroundNPct").ToDouble / 100.0
                        result.digestibility = crop.Variable("DefoliatedDigestibility").ToDouble
                Else
                        result.N_Conc = Default_N_Conc
                        result.digestibility = Default_Digestability
                End If
                Return result
        End Function

        'It would be nice to replce this with an ApSim "Cut" event call based on cut height but not implmented in AgPasture.
        'Do farmers really cut to the resudual or to a height?
        'Grazing could now use this function to harvest pasture for cows
        Public Function Harvest(ByVal CuttingResidual As Integer, ByVal loss As Double) As BioMass
                Dim cutDM As Double = Cover() - CuttingResidual
                Dim RemovalProportion = cutDM / Cover()
                If (DebugLevel > 0) Then
                        Console.WriteLine()
                        Console.WriteLine("DDRules.Harvest")
                        Console.WriteLine("     " & ApSim_SubPaddock.Name)
                        Console.WriteLine("     " & "+ Start Cover = " & Cover())
                        Console.WriteLine("     " & "- Residual    = " & CuttingResidual)
                        Console.WriteLine("     " & "= Cut DM      = " & cutDM)
                        Console.WriteLine("     " & "= Proportion  = " & RemovalProportion * 100 & "%")
                End If

                Dim RemovedMass As BioMass = New BioMass
                If cutDM > 0 Then
                        For Each crop As Component In ApSim_SubPaddock.Crops
                                Dim tempBioMass As BioMass = New BioMass
                                PastureMasses.TryGetValue(crop.Name, tempBioMass) 'this should really be checked, it should never fail but...
                                If (DebugLevel > 1) Then
                                        Console.WriteLine("DDRules.Harvest - " & tempBioMass.ToString())
                                End If
                                Dim tempRemovedMass = tempBioMass.Multiply(RemovalProportion)
                                crop.Publish("remove_crop_biomass", tempRemovedMass.toRemoveCropDmType())
                                ReturnDM(tempRemovedMass, loss)
                                RemovedMass = RemovedMass.Add(tempRemovedMass)
                                UpdateCovers()
                                PastureMasses.TryGetValue(crop.Name, tempBioMass) 'this should really be checked, it should never fail but...
                                If (DebugLevel > 1) Then
                                        Dim biomass As Double = crop.Variable("biomass").ToDouble
                                        Console.WriteLine("DDRules.Harvest - " & "Finish = " & biomass & " (includes stolons)")
                                        Console.WriteLine("DDRules.Harvest - " & "Finish = " & tempBioMass.ToString())
                                        Console.WriteLine("DDRules.Harvest - " & "Removed (ha) = " & RemovedMass.ToString())
                                        Console.WriteLine("DDRules.Harvest - " & "Removed Total= " & RemovedMass.Multiply(Area).ToString())
                                End If
                        Next
                End If
                Grazable = True       'flag paddock as "Growing"
                Return RemovedMass.Multiply(Area)
        End Function

        ' Return removed biomass to the SOM pools
        ' Being used during silage cutting for return of uncollected pasture mass
        ' Will be used as part of grazing event if trampling is to be included
        Public Sub ReturnDM(ByVal Mass As BioMass, ByVal PercentageReturn As Double)
                If (PercentageReturn > 0) Then
                        Dim DM As BiomassRemovedType = Mass.toBiomassRemovedType(PercentageReturn)
                        Dim SOM As Component = ApSim_SubPaddock.ComponentByType("surfaceom")
                        SOM.Publish("BiomassRemoved", DM)
                End If
        End Sub

#Region "Nutrient Return"
        ' kgN = kgN/paddock
        ' volume = l/paddock
        ' StockingDensity = head/ha/24hours
        Public Sub UrineApplication(ByVal kgN As Double, ByVal volume As Double, ByVal StockingDensity As Double)
                Dim kg As Double = kgN / Area
                Dim v As Double = volume / Area
                If (DebugLevel > 1) Then
                        If (kgN.ToString.Contains("NaN")) Then
                                Console.WriteLine("DDRules (debug) - " & "Urine: N = " & kgN.ToString & " V = " & volume.ToString & " A = " & Area.ToString)
                        End If
                        Console.WriteLine("DDRules (debug) - " & "Urine: N = " & kgN.ToString & " V = " & volume.ToString & " A = " & Area.ToString)
                End If
                If Not (UrinePatchComponent Is Nothing) Then ' use Val's new urine patch model is avalible
                        Dim urine As ApplyUrineType = New ApplyUrineType()
                        urine.AmountUrine = kg
                        urine.StockDensity = StockingDensity
                        urine.StockType = "DairyCow"
                        UrinePatchComponent.Publish("ApplyUrine", urine)
                Else    ' use simple fertiliser and irrigation events
                        Dim FertiliserA As Fertiliser = ApSim_SubPaddock.ComponentByType("Fertiliser")
                        Dim FertData As New FertiliserApplicationType()
                        FertData.Amount = kgN / Area
                        FertData.Type = "urea_N"
                        FertData.Depth = Default_Application_Depth
                        FertiliserA.Apply(FertData)

                        'Console.WriteLine("1 : Getting Irrigation Component")
                        Dim IrrigationA As Irrigation = ApSim_SubPaddock.ComponentByType("Irrigation")
                        Dim IrrData As New IrrigationApplicationType
                        IrrData.Amount = v / 10000 ' 20107003 - converting litres/ha to mm/ha
                        IrrData.source = New String() {} 'NB. can't be nothing
                        IrrData.Crop_Area = 0 ' used to calculation volume if source is given
                        IrrData.NO3 = 0
                        IrrData.NH4 = 0
                        IrrData.CL = 0
                        IrrData.time = ""
                        IrrData.Duration = 0
                        'Console.WriteLine("2 : Applying Irrigation")
                        IrrigationA.Apply(IrrData)
                        'Console.WriteLine("3 : Irrigation Applied")
                        N_Urine = kgN / Area
                End If
        End Sub

        Public Sub DungApplication(ByVal kgN As Double, ByVal kgDM As Double)
                If (kgN < 0) Or (kgDM < 0) Then
                        Console.WriteLine("******** WARNING ****************")
                        Console.WriteLine("Error: DDRules Dung Application")
                        Console.WriteLine("Negitive application amount found")
                        Console.WriteLine("     - kg N  = " + kgN)
                        Console.WriteLine("     - kg DM = " + kgDM)
                End If

                Dim kgN_ha As Double = kgN / Area
                Dim kgDM_ha As Double = kgDM / Area
                Dim dung As BiomassRemovedType = New BiomassRemovedType()
                dung.crop_type = "RuminantDung_PastureFed"
                dung.dm_type = New String() {"RuminantDung_PastureFed"}
                dung.dlt_crop_dm = New Single() {kgDM_ha}
                dung.dlt_dm_n = New Single() {kgN_ha}
                dung.dlt_dm_p = New Single() {kgDM_ha * (5.5 / 256.0)} 'Source: McDowell and Stewart (2005) Phosphorus in Fresh and Dry Dung of Grazing Dairy Cattle, Deer, and Sheep, J. Environ. Qual. 34:598-607 (2005). Table 1.
                dung.fraction_to_residue = New Single() {1.0}

                Dim SOM As Component = ApSim_SubPaddock.ComponentByType("surfaceom")
                SOM.Publish("BiomassRemoved", dung)
        End Sub
#End Region

        Public Sub UpdateCovers()
                PastureMasses.Clear()
                Pasture_Cover.clear()

                For Each Crop As Component In ApSim_SubPaddock.Crops 'counting all crops, this could cause issues with grazing allocation
                        Dim mass As BioMass = New BioMass()
                        mass.Name = Crop.Name
                        mass.gLeaf = Crop.Variable("leafgreenwt").ToDouble * 10 'convert from grams/m^2 to kg/ha
                        mass.gStem = Crop.Variable("stemgreenwt").ToDouble * 10
                        mass.dLeaf = Crop.Variable("leafsenescedwt").ToDouble * 10
                        mass.dStem = Crop.Variable("stemsenescedwt").ToDouble * 10

                        If Not (AgPasture Is Nothing) Then ' Note: this assumes AgPasture is the only crop model in the paddock
                                mass.N_Conc = Crop.Variable("AboveGroundNPct").ToDouble() / 100.0
                                Dim GrowthRate As Double = Crop.Variable("HerbageGrowthWt").ToDouble()
                                myAverageGrowthRate.Add(GrowthRate)
                                'Dim HerbageME As Double = Crop.Variable("HerbageME").ToDouble()
                                'Dim AboveGroundWt As Double = Crop.Variable("AboveGroundWt").ToDouble()
                                'Console.WriteLine(" AgPasture ME = " + (AboveGroundWt / HerbageME).ToString("0.00"))
                        Else
                                mass.N_Conc = Default_N_Conc
                        End If

                        Pasture_Cover = Pasture_Cover.Add(mass)
                        PastureMasses.Add(mass.Name, mass)
                        If (DebugLevel > 1) Then
                                Console.WriteLine("DDRules.updateCoverData - " & ApSim_SubPaddock.Name & "." & mass.ToString)
                        End If
                Next
                If (DebugLevel > 0) Then
                        Console.WriteLine("DDRules.updateCoverData - " & ApSim_SubPaddock.Name & ".Mass = " & Pasture_Cover.ToString)
                End If
        End Sub

#Region "Paddock Sorting"
        Private Class sortByCoverComparer : Implements System.Collections.Generic.IComparer(Of LocalPaddockType)
                Function Compare(ByVal x As LocalPaddockType, ByVal y As LocalPaddockType) As Integer Implements System.Collections.Generic.IComparer(Of LocalPaddockType).Compare
                        If (x.Cover < y.Cover) Then
                                Return 1
                        End If

                        If (x.Cover > y.Cover) Then
                                Return -1
                        End If

                        Return 0

                End Function
        End Class
        Private Class sortByIndexComparer : Implements System.Collections.Generic.IComparer(Of LocalPaddockType)
                Function Compare(ByVal x As LocalPaddockType, ByVal y As LocalPaddockType) As Integer Implements System.Collections.Generic.IComparer(Of LocalPaddockType).Compare
                        If (x.index < y.index) Then
                                Return -1
                        End If

                        If (x.index > y.index) Then
                                Return 1
                        End If

                        Return 0

                End Function
        End Class
        Public Shared Function getSortListByCover() As System.Collections.Generic.IComparer(Of LocalPaddockType)
                Return New sortByCoverComparer
        End Function
        Public Shared Function getSortListByIndex() As System.Collections.Generic.IComparer(Of LocalPaddockType)
                Return New sortByIndexComparer
        End Function
#End Region

#Region "Reporting Functions for Debuging"
        Public Overrides Function ToString() As String
                Dim result As String = "index " & index.ToString
                result = result & ", name " & ApSim_SubPaddock.Name
                result = result & ", area " & Area.ToString
                result = result & ", Status " & StatusCode()
                result = result & ", Cover " & Cover.ToString("0")
                Return result
        End Function
        Public Sub print(ByVal removed As RemoveCropBiomassdmType)
                If (DebugLevel > 0) Then
                        Console.WriteLine("DDRules.print (debug) - " & removed.pool.ToString())
                        For i As Integer = 0 To removed.part.Length - 1
                                Console.WriteLine("DDRules.print (debug) - " & removed.part(i) & ", " & removed.dlt(i).ToString())
                        Next
                End If
        End Sub
#End Region

        Public Function DM_Eaten() As Double
                Return DM_Grazed.DM_Total
        End Function

        Public Function AverageGrowthRate() As Double
                Return myAverageGrowthRate.Average
        End Function

        Public Function ME_Eaten() As Double
                Return DM_Grazed.getME_Total
        End Function
        Public Function N_Eaten() As Double
                Return DM_Grazed.N_Total
        End Function
        Public Function N_From_Feaces() As Double
                Return N_Feaces
        End Function
        Public Function N_From_Urine() As Double
                Return N_Urine
        End Function
        Public Function PastureME() As Double
                Return Pasture_Cover.getME()
        End Function
        Public Function AvalibleDryMater() As Double
                Return Math.Max(0, (Cover() - GrazingResidual) * Area)
        End Function
        Public Function AvalibleME() As Double
                Return AvalibleDryMater() * PastureME()
        End Function
        Public Function Cover() As Double
                Return Pasture_Cover.DM_Total
        End Function
        Public Function StatusCode() As String
                Return myStatus.ToString
        End Function

        Sub Irrigate(ByVal data As IrrigationApplicationType)
                Dim myIrrigation As Irrigation = ApSim_SubPaddock.ComponentByType("Irrigation")
                'myIrrigation.irrigation_efficiency = myIrrigation_efficiency 'don't know how to do this!
                data.Crop_Area = Area
                'Console.WriteLine("Irrigation " + Name)
                'Console.WriteLine("   mm    = " + data.Amount.ToString())
                'Console.WriteLine("   area  = " + data.Crop_Area.ToString())
                myIrrigation.Apply(data)
        End Sub

        Sub Apply(ByVal data As FertiliserApplicationType)
                Dim myFertiliser As Fertiliser = ApSim_SubPaddock.ComponentByType("Fertiliser")
                'Console.WriteLine("Fertiliser " + Name)
                'Console.WriteLine("   kg/ha = " + data.Amount.ToString())
                'Console.WriteLine("   mm    = " + data.Depth.ToString())
                'Console.WriteLine("   type  = " + data.Type)
                myFertiliser.Apply(data)
        End Sub

        Public Function PlantAvalibleWater(ByVal atDepth As Single) As Single
                Dim SoilWater As SoilWat
                SoilWater = ApSim_SubPaddock.ComponentByType("SoilWat")

                'Dim esw As Single = SoilWater.esw '  ApSim_SubPaddock.Variable("esw").ToDoubleArray()
                Dim sw_dep As Single() = SoilWater.sw_dep ' ApSim_SubPaddock.Variable("sw_dep").ToDoubleArray()
                Dim dul_dep As Single() = SoilWater.dul_dep ' ApSim_SubPaddock.Variable("ll15_dep").ToDoubleA
                Dim ll15_dep As Single() = SoilWater.ll15_dep ' ApSim_SubPaddock.Variable("ll15_dep").ToDoubleArray()
                Dim dLayer As Single() = SoilWater.dlayer ' ApSim_SubPaddock.Variable("ll15_dep").ToDoubleArray()
                'Dim PET As Single = SoilWater.eo
                Dim Stored As Single = 0.0
                Dim Possible As Single = 0.0
                Dim CumDepth As Single = 0.0
                For z As Integer = 0 To dLayer.Length - 1
                        CumDepth += dLayer(z)
                        Stored += (sw_dep(z) - ll15_dep(z)) * Math.Min(1.0, Math.Max(0.0, (1.0 - (CumDepth - atDepth) / dLayer(z))))
                        Possible += (dul_dep(z) - ll15_dep(z)) * Math.Min(1.0, Math.Max(0.0, (1.0 - (CumDepth - atDepth) / dLayer(z))))
                Next
                'Console.WriteLine("   Stored = " + Stored.ToString())
                'Console.WriteLine("   Possible = " + Possible.ToString())
                Return Stored / Possible
        End Function
End Class
