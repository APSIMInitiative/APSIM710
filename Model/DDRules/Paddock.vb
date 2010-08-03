
Public Class LocalPaddockType
        Public Enum PaddockStatus
                GR = 1  'Growing
                BG = 2 'Being grazed
                JG = 4 'Just grazed
                CL = 8  'Closed
        End Enum

        Dim debug As Boolean = False
        Private Default_N_Conc = 0.035
        Private index As Integer
        'Public TotalMass, TotalN As Double 'Pasture mass 
        Private ApSim_Pdk As PaddockType
        Private ApSim_ID As String
        Public Area As Double
        Private status As PaddockStatus
        Public GrazingResidual As Double = 0
        Dim DM_Grazed As BioMass = New BioMass()
        Dim DM_Cover As BioMass = New BioMass()
        Dim N_Feaces, C_Feaces, N_Urine As Double
        Public PastureMasses As Dictionary(Of String, BioMass) = New Dictionary(Of String, BioMass)

        '<[Event]()> Public Event BiomassRemoved(ByVal dung As BiomassRemovedType) 'testing event calling as per AgPasture
        Private UseUrinePatchModel As Boolean = False

        Public Sub New(ByVal index As Integer, ByRef paddock As PaddockType)
                ApSim_Pdk = paddock                                 'store a local pointer to the ApSim "Subpaddock" - I'm unsure if this will work as the reference may change inside ApSim at runtime
                Area = 1 'ApSim_Pdk.Variable("Pdk_area").ToDouble   'this might fail under 7.1r648 - can't find any dll reference
                ApSim_ID = paddock.Name                             'this might fail under 7.1r648 - can't find any dll reference
                status = PaddockStatus.GR              'all paddock grazable at initilisation time
                Me.index = index
                UseUrinePatchModel = Not (ApSim_Pdk.ComponentByName("UrinePatch") Is Nothing)
        End Sub

        Sub OnPrepare()
                N_Feaces = 0
                C_Feaces = 0
                N_Urine = 0
                DM_Grazed = New BioMass()
                '   UseUrinePatchModel = Not (ApSim_Pdk.ComponentByName("UrinePatch") Is Nothing)
        End Sub

        Function Graze(ByVal energyRequired As Double, ByVal GrazingResidual As Double) As BioMass
                'updateCoverData()
                update(GrazingResidual)

                Dim preGraze As New BioMass(DM_Cover)

                If (debug) Then
                        Console.WriteLine()
                        Console.WriteLine("  + Cover          = " & Cover())
                        Console.WriteLine("  - Residual       = " & GrazingResidual)
                        Console.WriteLine("  = Avalible DM    = " & AvalibleDryMater())
                        Console.WriteLine("  *          ME    = " & PastureME())
                        Console.WriteLine("  = Avalible ME    = " & AvalibleME())
                        Console.WriteLine("  - energyRequired = " & energyRequired)
                End If

                Dim Residual As Double = GrazingResidual
                Dim RemovedME As Double
                If (energyRequired < AvalibleME()) Then 'this paddock contains more ME that required...
                        'assue linear pasture removal/quality
                        Residual += ((AvalibleME() - energyRequired) / AvalibleME()) * AvalibleDryMater() 'only graze down to the required residual
                        RemovedME = energyRequired      'harvest the required amount
                        status = PaddockStatus.BG       ' might need to come back
                Else
                        RemovedME = AvalibleME()          'harvest all avalible DM/ME
                        status = PaddockStatus.JG
                End If
                If (debug) Then
                        Console.WriteLine("  = Grazing Residual = " & Residual)
                End If

                Dim result As BioMass = New BioMass()

                If (Residual < Cover()) Then
                        Dim MassRemoved As BioMass
                        For Each crop As CropType In ApSim_Pdk.Crops
                                MassRemoved = GrazePlant(crop, RemovedME)
                                MassRemoved.setME(PastureME)
                                result = result.Add(MassRemoved)
                        Next
                End If

                updateCoverData()
                If (debug) Then
                        Dim postGraze As New BioMass(DM_Cover)
                        Dim Removal As BioMass = preGraze.Subtract(postGraze)
                        Dim Test As BioMass = postGraze.Add(Removal)
                        Console.WriteLine(" Pre-Graze  = " & preGraze.ToString)
                        Console.WriteLine(" Post-Graze = " & postGraze.ToString)
                        Console.WriteLine(" Removal    = " & Removal.ToString)
                        Console.WriteLine(" Test       = " & Test.ToString)

                        Dim removed2 = result.DM_Total
                        Console.WriteLine("  N Removed   = " & Removal.N_Total)
                        Console.WriteLine("  N Concentration   = " & Removal.N_Conc)
                        Console.WriteLine("  ME   = " & result.getME())
                        Console.WriteLine("       = " & Removal.getME)
                        Console.WriteLine("  + Pasture Removed   = " & removed2)
                        Console.WriteLine("                      = " & Removal.DM_Total)
                        Console.WriteLine("  = Energy Removed    = " & result.getME_Total)
                        Console.WriteLine("                      = " & Removal.getME_Total)
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
                Return result
        End Function

        ' StockingDensity = head/ha/24hours
        Public Sub UrineApplication(ByVal kgN As Double, ByVal volume As Double, ByVal StockingDensity As Double)
                Dim Default_Application_Depth As Double = 300 'mm
                Dim kg As Double = kgN / Area
                Dim v As Double = volume / Area
                If (debug) Then
                        Console.WriteLine("Urine: N = " & kgN.ToString & " V = " & volume.ToString & " A = " & Area.ToString)
                End If

                If (UseUrinePatchModel) Then
                        ' use Val's new urine patch model
                        Dim urine As ApplyUrineType = New ApplyUrineType()
                        urine.AmountUrine = kg
                        urine.StockDensity = StockingDensity
                        urine.StockType = "DairyCow"
                        ApSim_Pdk.Publish("ApplyUrine", urine)
                Else
                        '  use simple fertiliser and irrigation events
                        ApSim_Pdk.Fertiliser.Apply(kgN / Area, Default_Application_Depth, "urea_N")
                        ApSim_Pdk.Irrigation.Apply(v / 10000) ' 20107003 - converting litres/ha to mm/ha
                        N_Urine = kgN / Area
                End If
        End Sub

        ' this doesn't seems to be going on?
        Public Sub DungApplication_old(ByVal kgN_ha As Double, ByVal kgC_ha As Double)
                Dim FOM As New FOMType
                FOM.amount = (kgN_ha + kgC_ha) / Area ' convert to grams per ha?
                FOM.N = kgN_ha / (kgN_ha + kgC_ha)
                FOM.C = kgC_ha / (kgN_ha + kgC_ha)
                If (debug) Then
                        Console.WriteLine("Dung: amount = " & FOM.amount.ToString & " N = " & FOM.N.ToString & " C = " & FOM.C.ToString)
                End If
                Dim C As VariableType = ApSim_Pdk.Component("surfaceom").Variable("surfaceom_wt")
                Dim c1 As Double = C.ToSingle
                ApSim_Pdk.Publish("add_surfaceom", FOM)
                C = ApSim_Pdk.Component("surfaceom").Variable("surfaceom_wt")
                Dim c2 As Double = C.ToSingle
                N_Feaces = kgN_ha
                C_Feaces = kgC_ha
        End Sub

        Public Sub DungApplication(ByVal kgN_ha As Double, ByVal kgC_ha As Double)
                Dim dung As BiomassRemovedType = New BiomassRemovedType()
                dung.crop_type = "manure"
                dung.dm_type = New String() {"manure"}
                dung.dlt_crop_dm = New Single() {kgN_ha + kgC_ha}
                dung.dlt_dm_n = New Single() {kgN_ha}
                dung.dlt_dm_p = New Single() {kgC_ha * (5.5 / 256.0)} 'Source: McDowell and Stewart (2005) Phosphorus in Fresh and Dry Dung of Grazing Dairy Cattle, Deer, and Sheep, J. Environ. Qual. 34:598-607 (2005). Table 1.
                dung.fraction_to_residue = New Single() {1.0}

                Dim SOM As ComponentType = ApSim_Pdk.Component("surfaceom")
                'Dim C As VariableType = SOM.Variable("surfaceom_wt")
                'Dim c1 As Double = C.ToSingle
                'ApSim_Pdk.Publish("BiomassRemoved", dung) nope!
                SOM.Publish("BiomassRemoved", dung)
                'RaiseEvent BiomassRemoved(dung) nope!
                'C = ApSim_Pdk.Component("surfaceom").Variable("surfaceom_wt")
                'Dim c2 As Double = C.ToSingle
                'Dim diff = c1 - c2
        End Sub

        ' to be call at the start of "OnProcess" to set up pasture covers etc
        Sub update(ByVal GrazingResidual As Integer)
                updateCoverData()
                Me.GrazingResidual = GrazingResidual
                updateStatus()
        End Sub

        Private Sub updateStatus()
                If (status = PaddockStatus.JG) Then
                        status = PaddockStatus.GR
                End If
        End Sub

        Public Property Closed() As Boolean
                Get
                        Return status = PaddockStatus.CL
                End Get
                Set(ByVal value As Boolean)
                        If (value) Then
                                status = PaddockStatus.CL
                        Else
                                status = PaddockStatus.GR
                        End If
                End Set
        End Property

        Public Function Harvest(ByVal Residual As Integer) As Double
                Dim GZ As GrazeType = New GrazeType()
                GZ.amount = Residual
                GZ.type = "residue"
                GZ.sender = "DDRules"
                Dim pre, post As Double

                pre = Cover()
                For Each crop As CropType In ApSim_Pdk.Crops
                        crop.Publish("graze", GZ)
                Next
                updateCoverData()
                Closed = False

                post = Cover()
                Return pre - post
        End Function

        Public Function Harvest2(ByVal Residual As Integer) As Double
                Dim cutDM As Double = Cover() - Residual
                If (debug) Then
                        Console.WriteLine()
                        Console.WriteLine("Paddock(" & ApSim_ID & "->Harvest(" & Residual & ")")
                        Console.WriteLine("     + Start Cover = " & Cover())
                        Console.WriteLine("     - Residual    = " & Residual)
                        Console.WriteLine("     = Cut DM      = " & cutDM)
                End If
                cutDM /= 10 'convert to g/m^2
                If cutDM > 0 Then
                        For Each crop As CropType In ApSim_Pdk.Crops
                                Dim b As BioMass = New BioMass
                                PastureMasses.TryGetValue(crop.name, b) 'this should really be checked, it should never fail but...
                                If (debug) Then
                                        Console.WriteLine(b.ToString())
                                End If
                                Dim Green2DeadSplit As Double = b.proportionDead()
                                ' work around because removal of dead material not functioning 20091110
                                Green2DeadSplit = 1 'assue all green
                                Dim cutGreenDM = cutDM * Green2DeadSplit
                                Dim cutDeadDM = cutDM - cutGreenDM
                                Dim greenRemoved As New RemoveCropDmdmType
                                greenRemoved.pool = "green"
                                greenRemoved.part = New String() {"leaf", "stem"}
                                Dim Leaf2StemSplit As Double = b.proportionGreenLeaf()
                                greenRemoved.dlt = New Double() {cutGreenDM * Leaf2StemSplit, cutGreenDM * (1 - Leaf2StemSplit)} '

                                'Dim deadRemoved As New RemoveCropDmdmType
                                'deadRemoved.pool = "senesced"
                                'deadRemoved.part = New String() {"leaf", "stem"}
                                'Leaf2StemSplit = b.proportionDeadLeaf()
                                'deadRemoved.dlt = New Double() {cutDeadDM * Leaf2StemSplit, cutDeadDM * (1 - Leaf2StemSplit)} '

                                Dim dmRemoved As New RemoveCropDmType
                                dmRemoved.dm = New RemoveCropDmdmType() {greenRemoved} ', deadRemoved}
                                'print(greenRemoved)
                                'print(deadRemoved)
                                crop.Publish("remove_crop_biomass", dmRemoved)
                                updateCoverData()
                                PastureMasses.TryGetValue(crop.name, b) 'this should really be checked, it should never fail but...
                                If (debug) Then
                                        Console.WriteLine("Finish = " & crop.biomass)
                                        Console.WriteLine("Finish = " & b.ToString())
                                End If

                        Next
                End If
                status = PaddockStatus.GR       'flag paddock as "Growing"
                Return cutDM
        End Function

        Public Sub print(ByVal deadRemoved As RemoveCropDmdmType)
                If (debug) Then
                        Console.WriteLine("*** " & deadRemoved.pool.ToString())
                        For i As Integer = 0 To deadRemoved.part.Length - 1
                                Console.WriteLine("***    " & deadRemoved.part(i) & ", " & deadRemoved.dlt(i).ToString())
                        Next
                End If
        End Sub

        Public Sub updateCoverData()
                PastureMasses.Clear()
                DM_Cover = New BioMass()

                Dim gLeaf, gStem, dLeaf, dStem As VariableType
                Dim tempStr As String
                For Each Crop As CropType In ApSim_Pdk.Crops 'counting all crops, this could cause issues with grazing allocation
                        tempStr = Crop.name
                        gLeaf = Crop.Variable("leafgreenwt")
                        gStem = Crop.Variable("stemgreenwt")
                        dLeaf = Crop.Variable("leafsenescedwt")
                        dStem = Crop.Variable("stemsenescedwt")
                        'End If
                        Dim mass As BioMass = New BioMass()
                        mass.Name = Crop.name
                        mass.gLeaf = gLeaf.ToDouble * 10 'convert from grams/m^2 to kg/ha
                        mass.gStem = gStem.ToDouble * 10
                        mass.dLeaf = dLeaf.ToDouble * 10
                        mass.dStem = dStem.ToDouble * 10
                        If (debug) Then
                                Console.WriteLine("  " & ApSim_ID & ".Mass = " & mass.ToString)
                        End If

                        Dim isAgPasture As Boolean = False 'need to develop this test
                        If (isAgPasture) Then
                                Dim tempCover As Double = Crop.Variable("AboveGroundWt").ToDouble() 'this values is different from cover calculation above
                                Dim tempN As Double = Crop.Variable("AboveGroundN").ToDouble()
                                Dim concN As Double = tempN / tempCover
                                mass.N_Conc = concN
                        Else
                                mass.N_Conc = Default_N_Conc
                        End If
                        PastureMasses.Add(mass.Name, mass)
                        DM_Cover = DM_Cover.Add(mass)
                Next

                'Calculate ME here?
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

        Public Overrides Function ToString() As String
                Dim result As String = "index " & index.ToString
                result = result & ", name " & ApSim_ID.ToString
                result = result & ", area " & Area.ToString
                result = result & ", Status " & StatusCode()
                result = result & ", Cover " & Cover.ToString("0")
                Return result
        End Function

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
                Return DM_Cover.getME()
        End Function
        Public Function AvalibleDryMater() As Double
                Return Math.Max(0, (Cover() - GrazingResidual) * Area)
        End Function

        Public Function AvalibleME() As Double
                Return AvalibleDryMater() * PastureME()
        End Function
        Public Function Cover() As Double
                Return DM_Cover.DM_Total
        End Function
        Public Function StatusCode() As String
                Return status.ToString
        End Function
End Class
