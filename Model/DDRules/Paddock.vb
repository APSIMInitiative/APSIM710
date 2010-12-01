
Public Class LocalPaddockType
        Public Enum PaddockStatus
                G = 1  'Growing
                BG = 2 'Being grazed
                JG = 4 'Just grazed
                CL = 8  'Closed
                NA = 19  'Not avalible for grazing
        End Enum

        Public DebugLevel As Integer = 2 '0==none, 1==brief, 2==verbose

        Private Default_N_Conc = 0.035
        Private index As Integer
        'Public TotalMass, TotalN As Double 'Pasture mass 
        Private ApSim_SubPaddock As PaddockType
        Public Area As Double
        Private status As PaddockStatus
        Public GrazingResidual As Double = 0
        Dim DM_Grazed As BioMass = New BioMass()
        Dim Pasture_Cover As BioMass = New BioMass()
        Dim N_Feaces, C_Feaces, N_Urine As Double
        Public PastureMasses As Dictionary(Of String, BioMass) = New Dictionary(Of String, BioMass)
        Public Counter As Integer = 0

        Private patch As ComponentType
        Private AgPasture As ComponentType

        Public Sub New(ByVal index As Integer, ByRef paddock As PaddockType, ByVal PaddockArea As Double)
                ApSim_SubPaddock = paddock                              'store a local pointer to the ApSim "Subpaddock" 
                Area = PaddockArea
                Grazable = True                               'paddock grazable at initilisation time
                Me.index = index                                        'sort origional paddock position in the simulation for sorting
                patch = ApSim_SubPaddock.ComponentByName("UrinePatch")
                AgPasture = ApSim_SubPaddock.Component("AgPasture")
        End Sub

        Public Sub New(ByVal index As Integer, ByRef paddock As PaddockType)
                Me.New(index, paddock, 1.0)
        End Sub

        Sub OnPrepare()
                N_Feaces = 0
                C_Feaces = 0
                N_Urine = 0
                DM_Grazed = New BioMass()
                If (status = PaddockStatus.JG) Then
                        Grazable = True
                End If
        End Sub

        Public Sub setJustGrazed()
                status = PaddockStatus.JG
        End Sub

        Public Sub setBeingGrazed()
                status = PaddockStatus.BG
        End Sub

        Public Property Closed() As Boolean
                Get
                        Return status = PaddockStatus.CL
                End Get
                Set(ByVal value As Boolean)
                        If (value) Then
                                status = PaddockStatus.CL
                        Else
                                Grazable = True
                        End If
                End Set
        End Property

        Public Property Grazable() As Boolean
                Get
                        Return status <> PaddockStatus.NA
                End Get
                Set(ByVal value As Boolean)
                        If (value) Then
                                status = PaddockStatus.G
                        Else
                                status = PaddockStatus.NA
                        End If
                End Set
        End Property

        Function Graze(ByVal energyRequired As Double, ByVal GrazingResidual As Double) As BioMass
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
                If (energyRequired < AvalibleME()) Then 'this paddock contains more ME that required...
                        'assue linear pasture removal/quality
                        Dim aME As Double = AvalibleME()
                        Dim aDM As Double = AvalibleDryMater()
                        Dim ratio = (aME - energyRequired) / aME
                        Dim gr As Double = ratio * aDM
                        GrazingResidual += ((AvalibleME() - energyRequired) / AvalibleME()) * (Cover() - GrazingResidual) 'only graze down to the required residual
                        RemovedME = energyRequired      'harvest the required amount
                Else
                        RemovedME = AvalibleME()          'harvest all avalible DM/ME
                        'status = PaddockStatus.JG
                End If
                setBeingGrazed()       ' might need to come back

                If (DebugLevel > 0) Then
                        Console.WriteLine("DDRules (debug) - " & "  = Grazing Residual = " & GrazingResidual)
                End If

                Dim result As BioMass = New BioMass()
                If (GrazingResidual < Cover()) Then
                        Dim MassRemoved As BioMass
                        For Each crop As CropType In ApSim_SubPaddock.Crops
                                Dim tempBioMass As BioMass = New BioMass
                                PastureMasses.TryGetValue(crop.name, tempBioMass) 'this should really be checked, it should never fail but...
                                Dim proportion As Double = tempBioMass.getME_Total / PreGrazeMass.getME_Total
                                If (ApSim_SubPaddock.Crops.Count = 1 And proportion <> 1) Then
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

        Function GrazePlant(ByVal crop As CropType, ByVal RemovedME As Double) As BioMass
                Dim result As BioMass = New BioMass
                PastureMasses.TryGetValue(crop.name, result) 'this should really be checked, but it should never fail :)
                Dim percentageRemoval As Double = RemovedME / result.getME_Total
                result = result.Multiply(percentageRemoval)
                crop.Publish("remove_crop_biomass", result.toRemoveCropDmType)

                If Not (AgPasture Is Nothing) Then
                        result.N_Conc = crop.Variable("AboveGroundNPct").ToDouble / 100.0
                        result.digestibility = crop.Variable("DefoliatedDigestibility").ToDouble
                Else
                        result.N_Conc = Default_N_Conc
                        result.digestibility = 0.68
                End If
                Return result
        End Function

        'It would be nice to replce this with an ApSim "Cut" event call based on cut height but not implmented in AgPasture.
        'Do farmers really cut to the resudual or to a height?
        ' "loss" parameter defines the amount of pasture cut but not collected for silage
        ' this is returned to paddock surface organic matter
        ' TODO: this is in KgDM/ha. Nedd to check this is being handled correctly since inclusion of farm area.
        ' TODO: return an instance of BioMass rather than pure DM figure
        Public Function Harvest_Old(ByVal CuttingResidual As Integer, ByVal loss As Double) As Double
                Dim cutDM As Double = Cover() - CuttingResidual
                If (DebugLevel > 0) Then
                        Console.WriteLine()
                        Console.WriteLine("DDRules.Harvest")
                        Console.WriteLine("     " & ApSim_SubPaddock.Name)
                        Console.WriteLine("     " & "+ Start Cover = " & Cover())
                        Console.WriteLine("     " & "- Residual    = " & CuttingResidual)
                        Console.WriteLine("     " & "= Cut DM      = " & cutDM)
                End If

                cutDM /= 10 'convert kg/ha to g/m^2
                If cutDM > 0 Then
                        For Each crop As CropType In ApSim_SubPaddock.Crops
                                Dim tempBioMass As BioMass = New BioMass
                                PastureMasses.TryGetValue(crop.name, tempBioMass) 'this should really be checked, it should never fail but...
                                If (DebugLevel > 1) Then
                                        Console.WriteLine("DDRules.Harvest - " & tempBioMass.ToString())
                                End If
                                Dim Green2DeadSplit As Double = 1 - tempBioMass.proportionDead()
                                Dim cutGreenDM = cutDM * Green2DeadSplit
                                Dim cutDeadDM = cutDM - cutGreenDM
                                Dim greenRemoved As New RemoveCropDmdmType
                                greenRemoved.pool = "green"
                                greenRemoved.part = New String() {"leaf", "stem"}
                                Dim Leaf2StemSplit As Double = tempBioMass.proportionGreenLeaf()
                                greenRemoved.dlt = New Double() {cutGreenDM * Leaf2StemSplit, cutGreenDM * (1 - Leaf2StemSplit)} '

                                Dim deadRemoved As New RemoveCropDmdmType
                                deadRemoved.pool = "dead"
                                deadRemoved.part = New String() {"leaf", "stem"}
                                Leaf2StemSplit = tempBioMass.proportionDeadLeaf()
                                deadRemoved.dlt = New Double() {cutDeadDM * Leaf2StemSplit, cutDeadDM * (1 - Leaf2StemSplit)} '

                                Dim dmRemoved As New RemoveCropDmType
                                dmRemoved.dm = New RemoveCropDmdmType() {greenRemoved, deadRemoved}
                                If (DebugLevel > 1) Then
                                        print(greenRemoved)
                                        print(deadRemoved)
                                End If
                                crop.Publish("remove_crop_biomass", dmRemoved)
                                ReturnDM(tempBioMass, loss)

                                UpdateCovers()
                                PastureMasses.TryGetValue(crop.name, tempBioMass) 'this should really be checked, it should never fail but...
                                If (DebugLevel > 1) Then
                                        Console.WriteLine("DDRules.Harvest - " & "Finish = " & crop.biomass)
                                        Console.WriteLine("DDRules.Harvest - " & "Finish = " & tempBioMass.ToString())
                                End If
                        Next
                End If
                Grazable = True       'flag paddock as "Growing"
                Return cutDM * 10 * (1 - loss)
        End Function

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
                        For Each crop As CropType In ApSim_SubPaddock.Crops
                                Dim tempBioMass As BioMass = New BioMass
                                PastureMasses.TryGetValue(crop.name, tempBioMass) 'this should really be checked, it should never fail but...
                                If (DebugLevel > 1) Then
                                        Console.WriteLine("DDRules.Harvest - " & tempBioMass.ToString())
                                End If
                                Dim tempRemovedMass = tempBioMass.Multiply(RemovalProportion)
                                crop.Publish("remove_crop_biomass", tempRemovedMass.toRemoveCropDmType())
                                ReturnDM(tempRemovedMass, loss)
                                RemovedMass = RemovedMass.Add(tempRemovedMass)
                                UpdateCovers()
                                PastureMasses.TryGetValue(crop.name, tempBioMass) 'this should really be checked, it should never fail but...
                                If (DebugLevel > 1) Then
                                        Console.WriteLine("DDRules.Harvest - " & "Finish = " & crop.biomass & " (includes stolons)")
                                        Console.WriteLine("DDRules.Harvest - " & "Finish = " & tempBioMass.ToString())
                                End If
                        Next
                End If
                Grazable = True       'flag paddock as "Growing"
                Return RemovedMass
        End Function

        ' return removed biomass to the SOM pools
        ' Being used during silage cutting for return of uncollected pasture mass
        ' Will be used as part of grazing event if trampling is to be included
        Public Sub ReturnDM(ByVal Mass As BioMass, ByVal PercentageReturn As Double)
                If (PercentageReturn > 0) Then
                        Dim DM As BiomassRemovedType = Mass.toBiomassRemovedType(PercentageReturn)
                        Dim SOM As ComponentType = ApSim_SubPaddock.Component("surfaceom")
                        SOM.Publish("BiomassRemoved", DM)
                End If
        End Sub

#Region "Nutrient Return"
        ' kgN = kgN/paddock
        ' volume = l/paddock
        ' StockingDensity = head/ha/24hours
        Public Sub UrineApplication(ByVal kgN As Double, ByVal volume As Double, ByVal StockingDensity As Double)
                Dim Default_Application_Depth As Double = 300 'mm
                Dim kg As Double = kgN / Area
                Dim v As Double = volume / Area
                If (DebugLevel > 1) Then
                        If (kgN.ToString.Contains("NaN")) Then
                                Console.WriteLine("DDRules (debug) - " & "Urine: N = " & kgN.ToString & " V = " & volume.ToString & " A = " & Area.ToString)
                        End If
                        Console.WriteLine("DDRules (debug) - " & "Urine: N = " & kgN.ToString & " V = " & volume.ToString & " A = " & Area.ToString)
                End If
                If Not (patch Is Nothing) Then ' use Val's new urine patch model is avalible
                        Dim urine As ApplyUrineType = New ApplyUrineType()
                        urine.AmountUrine = kg
                        urine.StockDensity = StockingDensity
                        urine.StockType = "DairyCow"
                        patch.Publish("ApplyUrine", urine)
                Else    ' use simple fertiliser and irrigation events
                        ApSim_SubPaddock.Fertiliser.Apply(kgN / Area, Default_Application_Depth, "urea_N")
                        ApSim_SubPaddock.Irrigation.Apply(v / 10000) ' 20107003 - converting litres/ha to mm/ha
                        N_Urine = kgN / Area
                End If
        End Sub

        Public Sub DungApplication(ByVal kgN_ha As Double, ByVal kgDM_ha As Double)
                If (kgN_ha < 0) Or (kgDM_ha < 0) Then
                        Console.WriteLine("******** WARNING ****************")
                        Console.WriteLine("Error: DDRules Dung Application")
                        Console.WriteLine("Negitive application amount found")
                        Console.WriteLine("     - kg N per ha = " + kgN_ha)
                        Console.WriteLine("     - kg DM per ha = " + kgDM_ha)
                End If

                Dim dung As BiomassRemovedType = New BiomassRemovedType()
                dung.crop_type = "RuminantDung_PastureFed"
                dung.dm_type = New String() {"RuminantDung_PastureFed"}
                dung.dlt_crop_dm = New Single() {kgDM_ha}
                dung.dlt_dm_n = New Single() {kgN_ha}
                dung.dlt_dm_p = New Single() {kgDM_ha * (5.5 / 256.0)} 'Source: McDowell and Stewart (2005) Phosphorus in Fresh and Dry Dung of Grazing Dairy Cattle, Deer, and Sheep, J. Environ. Qual. 34:598-607 (2005). Table 1.
                dung.fraction_to_residue = New Single() {1.0}

                Dim SOM As ComponentType = ApSim_SubPaddock.Component("surfaceom")
                SOM.Publish("BiomassRemoved", dung)
        End Sub
#End Region

        Public Sub UpdateCovers()
                PastureMasses.Clear()
                Pasture_Cover.clear()

                For Each Crop As CropType In ApSim_SubPaddock.Crops 'counting all crops, this could cause issues with grazing allocation
                        Dim mass As BioMass = New BioMass()
                        mass.Name = Crop.name
                        mass.gLeaf = Crop.Variable("leafgreenwt").ToDouble * 10 'convert from grams/m^2 to kg/ha
                        mass.gStem = Crop.Variable("stemgreenwt").ToDouble * 10
                        mass.dLeaf = Crop.Variable("leafsenescedwt").ToDouble * 10
                        mass.dStem = Crop.Variable("stemsenescedwt").ToDouble * 10

                        If Not (AgPasture Is Nothing) Then ' Note: this assumes AgPasture is the only crop model in the paddock
                                mass.N_Conc = Crop.Variable("AboveGroundNPct").ToDouble() / 100.0
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
        Private Class sortByCoverComparerNew : Implements System.Collections.Generic.IComparer(Of LocalPaddockType)
                Function Compare(ByVal x As LocalPaddockType, ByVal y As LocalPaddockType) As Integer Implements System.Collections.Generic.IComparer(Of LocalPaddockType).Compare
                        'Additional code to make sure the previous "being grazed" paddock gets grazed first - needs more thought "need to test if paddock has been grazed down properly"
                        'If (x.status = PaddockStatus.BG And y.status <> PaddockStatus.BG) Then
                        '        Return 1
                        'ElseIf (x.status <> PaddockStatus.BG And y.status = PaddockStatus.BG) Then
                        '        Return -1
                        'End If

                        If (x.Cover < y.Cover) Then
                                Return 1
                        End If

                        If (x.Cover > y.Cover) Then
                                Return -1
                        End If

                        Return 0

                End Function
        End Class
        Private Class sortByIndexComparerNew : Implements System.Collections.Generic.IComparer(Of LocalPaddockType)
                Function Compare(ByVal x As LocalPaddockType, ByVal y As LocalPaddockType) As Integer Implements System.Collections.Generic.IComparer(Of LocalPaddockType).Compare
                        If (x.index < y.index) Then
                                Return 1
                        End If

                        If (x.index > y.index) Then
                                Return -1
                        End If

                        Return 0

                End Function
        End Class
        Public Shared Function getSortListByCover() As System.Collections.Generic.IComparer(Of LocalPaddockType)
                Return New sortByCoverComparerNew
        End Function
        Public Shared Function getSortListByIndex() As System.Collections.Generic.IComparer(Of LocalPaddockType)
                Return New sortByIndexComparerNew
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

        Public Sub print(ByVal removed As RemoveCropDmdmType)
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
                Return status.ToString
        End Function
End Class
