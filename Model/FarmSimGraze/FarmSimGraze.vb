Imports CSGeneral


Public Class FarmSimGraze
    Inherits Instance

    Public MyPaddock As PaddockType

    <Input()> Private UI_FarmType As String


    <Input()> Private UI_BeefPercentage As Single   ' beef percentage on a per head basis
    Private SUBeefPercentage As Single

    <Input()> Private day As Integer

    Private DayOfYear_x As Double() = {0, 15, 46, 75, 106, 136, 167, 197, 228, 259, 289, 320, 350, 366}
    Private TriggerDM_y As Double() = {2200, 2200, 2200, 2000, 1800, 1800, 1800, 1800, 1900, 2000, 2200, 2200, 2200, 2200}
    Private ResidualDM_y As Double() = {1200, 1200, 1200, 1000, 600, 600, 600, 600, 700, 900, 1200, 1200, 1200, 1200}
    <Output()> Public DM_trigger As Double = 1800.0   ' consider changing to rotation rate though
    <Output()> Public DM_residual As Double = 600.0
    Private DM_grazable As Boolean = False  ' got error when tried to report this
    <Output()> Public DryMatter As Double = 0.0
    <Output()> Public DryMatterPostGrazing As Double = 0.0
    <Output()> Public DryMatter_Intake As Double = 0.0
    <Output()> Public PropDMRemoval As Double = 0.0
    <Output()> Public N_Intake As Double = 0.0
    <Output()> Public N_Urine As Double = 0.0
    <Output()> Public N_Dung As Double = 0.0
    <Output()> Public N_Product As Double = 0.0
    <Output()> Public AnimalType As String = ""

    <Output()> Public IntakePerHead As Double = 0.0
    <Output()> Public EffectiveStockDensity As Double = 0.0



    Public GreenLeaf As Double = 0.0
    Public GreenStem As Double = 0.0
    Public DeadLeaf As Double = 0.0
    Public DeadStem As Double = 0.0


    Private Intake_y As Double() = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
    Private N2Product_y As Double() = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
    Private N2Dung_y As Double() = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
    Private N2Urine_y As Double() = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}


    Private SheepIntake_y As Double() = {1.5, 1, 1, 1.4, 1.4, 1, 1, 1, 1.15, 1.3, 2.5, 2.75, 3, 1.5}
    Private SheepN2Product_y As Double() = {0.1, 0.1, 0.1, 0.1, 0.075, 0.075, 0.05, 0.05, 0.05, 0.075, 0.1, 0.1, 0.1, 0.1}
    Private SheepN2Dung_y As Double() = {0.36, 0.36, 0.36, 0.36, 0.37, 0.37, 0.38, 0.38, 0.38, 0.37, 0.36, 0.36, 0.36, 0.36}
    Private SheepN2Urine_y As Double() = {0.54, 0.54, 0.54, 0.54, 0.555, 0.555, 0.57, 0.57, 0.57, 0.555, 0.54, 0.54, 0.54, 0.54}

    Private LambIntake_y As Double() = {0.9, 1.1, 1.3, 1.5, 1.7, 1.8, 2, 2.2, 2.4, 0.3, 0.5, 0.7, 0.8, 0.9}
    Private LambN2Product_y As Double() = {0.15, 0.15, 0.15, 0.15, 0.125, 0.125, 0.1, 0.1, 0.1, 0.125, 0.15, 0.15, 0.15, 0.15}
    Private LambN2Dung_y As Double() = {0.34, 0.34, 0.34, 0.34, 0.35, 0.35, 0.36, 0.36, 0.36, 0.35, 0.34, 0.34, 0.34, 0.34}
    Private LambN2Urine_y As Double() = {0.51, 0.51, 0.51, 0.51, 0.525, 0.525, 0.54, 0.54, 0.54, 0.525, 0.51, 0.51, 0.51, 0.51}

    Private BeefIntake_y As Double() = {5.5, 5.5, 6, 6, 5.5, 5, 5, 5, 5, 5, 5, 5.25, 5.5, 5.5}
    Private BeefN2Product_y As Double() = {0.2, 0.2, 0.2, 0.2, 0.15, 0.15, 0.1, 0.1, 0.1, 0.15, 0.15, 0.2, 0.2, 0.2}
    Private BeefN2Dung_y As Double() = {0.32, 0.32, 0.32, 0.32, 0.34, 0.34, 0.36, 0.36, 0.36, 0.34, 0.34, 0.32, 0.32, 0.32}
    Private BeefN2Urine_y As Double() = {0.48, 0.48, 0.48, 0.48, 0.51, 0.51, 0.54, 0.54, 0.54, 0.51, 0.51, 0.48, 0.48, 0.48}

    Private DairyCowIntake_y As Double() = {15, 15, 14.5, 13.5, 12.1, 8.5, 6.5, 6.5, 10, 13.5, 15, 15, 15, 15}
    Private DairyCowN2Product_y As Double() = {0.3, 0.3, 0.3, 0.25, 0.2, 0.15, 0.1, 0.1, 0.15, 0.25, 0.3, 0.3, 0.3, 0.36}
    Private DairyCowN2Dung_y As Double() = {0.28, 0.28, 0.28, 0.3, 0.32, 0.34, 0.36, 0.36, 0.34, 0.3, 0.28, 0.28, 0.28, 0.256}
    Private DairyCowN2Urine_y As Double() = {0.42, 0.42, 0.42, 0.45, 0.48, 0.51, 0.54, 0.54, 0.51, 0.45, 0.42, 0.42, 0.42, 0.384}





    <EventHandler()> Public Sub OnInit2()

        Console.WriteLine("Initialising FarmSimGraze")
        SUBeefPercentage = (UI_BeefPercentage * 6.0) / (UI_BeefPercentage * 6.0 + (100 - UI_BeefPercentage))
        If UI_FarmType = "Dairy" Then
            AnimalType = "DairyCow"
            For i As Integer = 0 To 13
                Intake_y(i) = DairyCowIntake_y(i)
                N2Product_y(i) = DairyCowN2Product_y(i)
                N2Dung_y(i) = DairyCowN2Dung_y(i)
                N2Urine_y(i) = DairyCowN2Urine_y(i)
            Next
        ElseIf UI_FarmType = "SheepAndBeef" Then
            AnimalType = "SheepBeef"
            Dim PropBeef As Double = UI_BeefPercentage / 100.0
            Dim PropSheep As Double = 1.0 - PropBeef
            For i As Integer = 0 To 13
                Intake_y(i) = (BeefIntake_y(i) * PropBeef) + (SheepIntake_y(i) * PropSheep)
                N2Product_y(i) = (BeefN2Product_y(i) * PropBeef) + (SheepN2Product_y(i) * PropSheep)
                N2Dung_y(i) = (BeefN2Dung_y(i) * PropBeef) + (SheepN2Dung_y(i) * PropSheep)
                N2Urine_y(i) = (BeefN2Urine_y(i) * PropBeef) + (SheepN2Urine_y(i) * PropSheep)
            Next
        ElseIf UI_FarmType = "IntensiveArable" Or "TraditionalArable" Then
            AnimalType = "Lamb"
            For i As Integer = 0 To 13
                Intake_y(i) = LambIntake_y(i)
                N2Product_y(i) = LambN2Product_y(i)
                N2Dung_y(i) = LambN2Dung_y(i)
                N2Urine_y(i) = LambN2Urine_y(i)
            Next
        Else
            Throw New Exception("Farm type not allowed")
        End If
        Console.WriteLine("Finished Initialising FarmSimGraze")

    End Sub

    <EventHandler()> Public Sub OnPrepare()

        MyPaddock = New PaddockType(Me)

        'find grazing trigger and residual
        DM_trigger = LinearInterp(day, DayOfYear_x, TriggerDM_y)
        DM_residual = LinearInterp(day, DayOfYear_x, ResidualDM_y)

        'find amount of DM in paddock and graze if at trigger
        If (MyPaddock.ComponentByName("ryegrass_clover") Is Nothing) Then
            Throw New Exception("No pasture module to be grazed")
        Else
            GetDM()  'DM = MyPaddock.ComponentByName("ryegrass_clover").Variable("AboveGroundWt").ToDouble
            If DryMatter >= DM_trigger Then
                DM_grazable = True
                DryMatter_Intake = DryMatter - DM_residual
                PropDMRemoval = DryMatter_Intake / DryMatter
                N_Intake = DryMatter_Intake / DryMatter * MyPaddock.ComponentByName("ryegrass_clover").Variable("AboveGroundN").ToDouble

                GrazeDM(PropDMRemoval)
                GetDMPost()
            Else
                DM_grazable = False
                DryMatter_Intake = 0.0
                N_Intake = 0.0
            End If
        End If

        'calculate effective stocking density
        If DM_grazable Then
            IntakePerHead = LinearInterp(day, DayOfYear_x, Intake_y)
            EffectiveStockDensity = DryMatter_Intake / IntakePerHead
            N_Urine = LinearInterp(day, DayOfYear_x, N2Urine_y) * N_Intake
            N_Dung = LinearInterp(day, DayOfYear_x, N2Dung_y) * N_Intake
            N_Product = LinearInterp(day, DayOfYear_x, N2Product_y) * N_Intake

            UrineApplication(N_Urine, EffectiveStockDensity, AnimalType)
            DungApplication(N_Dung, DryMatter_Intake * 0.85) ' assume 85% digestibilty
        Else
            IntakePerHead = 0.0
            EffectiveStockDensity = 0.0
            N_Urine = 0.0
            N_Dung = 0.0
            N_Product = 0.0
        End If

    End Sub

    Public Sub GetDM()

        'DM = MyPaddock.ComponentByName("ryegrass_clover").Variable("AboveGroundWt").ToDouble

        DryMatter = 0.0
        GreenLeaf = 0.0
        GreenStem = 0.0
        DeadLeaf = 0.0
        DeadStem = 0.0

        For Each Crop As CropType In MyPaddock.Crops 'counting all crops, this could cause issues with grazing allocation
            GreenLeaf += Crop.Variable("leafgreenwt").ToDouble * 10.0   'convert to Kg DM /ha
            GreenStem += Crop.Variable("stemgreenwt").ToDouble * 10.0   'convert to Kg DM /ha
            DeadLeaf += Crop.Variable("leafsenescedwt").ToDouble * 10.0 'convert to Kg DM /ha
            DeadStem += Crop.Variable("stemsenescedwt").ToDouble * 10.0 'convert to Kg DM /ha
            DryMatter += GreenLeaf + GreenStem + DeadLeaf + DeadStem
        Next

    End Sub
    Public Sub GetDMPost()

        'DM = MyPaddock.ComponentByName("ryegrass_clover").Variable("AboveGroundWt").ToDouble

        DryMatterPostGrazing = 0.0
        Dim GreenLeaf = 0.0
        Dim GreenStem = 0.0
        Dim DeadLeaf = 0.0
        Dim DeadStem = 0.0

        For Each Crop As CropType In MyPaddock.Crops 'counting all crops, this could cause issues with grazing allocation
            GreenLeaf += Crop.Variable("leafgreenwt").ToDouble * 10.0   'convert to Kg DM /ha
            GreenStem += Crop.Variable("stemgreenwt").ToDouble * 10.0   'convert to Kg DM /ha
            DeadLeaf += Crop.Variable("leafsenescedwt").ToDouble * 10.0 'convert to Kg DM /ha
            DeadStem += Crop.Variable("stemsenescedwt").ToDouble * 10.0 'convert to Kg DM /ha
            DryMatterPostGrazing += GreenLeaf + GreenStem + DeadLeaf + DeadStem
        Next

    End Sub

    Public Sub GrazeDM(ByVal PropDMRemoval As Double)

        'Dim GZ As GrazeType = New GrazeType()
        'GZ.amount = DryMatter_Intake
        'GZ.type = "removal"
        'GZ.sender = "FarmSimGraze"
        'MyPaddock.Publish("graze", GZ)

        Dim GreenRemoveCropDmDm As New RemoveCropDmdmType
        GreenRemoveCropDmDm.pool = "green"
        GreenRemoveCropDmDm.part = New String() {"leaf", "stem"}
        GreenRemoveCropDmDm.dlt = New Double() {PropDMRemoval * GreenLeaf / 10.0, PropDMRemoval * GreenStem / 10.0}

        Dim DeadRemoveCropDmDm As New RemoveCropDmdmType
        DeadRemoveCropDmDm.pool = "Dead"
        DeadRemoveCropDmDm.part = New String() {"leaf", "stem"}
        DeadRemoveCropDmDm.dlt = New Double() {PropDMRemoval * DeadLeaf / 10.0, PropDMRemoval * DeadStem / 10.0}

        Dim MyRemoveCropDM As New RemoveCropDmType
        MyRemoveCropDM.dm = New RemoveCropDmdmType() {GreenRemoveCropDmDm, DeadRemoveCropDmDm}

        MyPaddock.Publish("remove_crop_biomass", MyRemoveCropDM)

    End Sub


    Private Sub UrineApplication(ByVal UrineN As Double, ByVal StockDensity As Double, ByVal StockType As String)
        Dim ApplyUrineData As New ApplyUrineType
        ApplyUrineData.AmountUrine = N_Urine
        ApplyUrineData.StockDensity = EffectiveStockDensity
        ApplyUrineData.StockType = AnimalType
        MyPaddock.Publish("ApplyUrine", ApplyUrineData)

    End Sub


    Private Sub DungApplication(ByVal DungN As Double, ByVal DungC As Double)

        Dim DungData As BiomassRemovedType = New BiomassRemovedType()

        DungData.crop_type = "manure"
        DungData.dm_type = New String() {"manure"}
        DungData.dlt_crop_dm = New Single() {DungN + DungC}
        DungData.dlt_dm_n = New Single() {DungN}
        DungData.dlt_dm_p = New Single() {DungC * (5.5 / 256.0)} 'Source: McDowell and Stewart (2005) Phosphorus in Fresh and Dry Dung of Grazing Dairy Cattle, Deer, and Sheep, J. Environ. Qual. 34:598-607 (2005). Table 1.
        DungData.fraction_to_residue = New Single() {1.0}

        Dim SOM As ComponentType = MyPaddock.Component("surfaceom")
        MyPaddock.Publish("BiomassRemoved", DungData)

    End Sub

    <Output()> Public ReadOnly Property IsGrazable() As String
        Get
            Return DM_grazable.ToString
        End Get
    End Property


    Public Function LinearInterp(ByVal x As Single, ByVal lookup_x() As Double, ByVal lookup_y() As Double) As Double
        Dim DidInterpolate As Boolean = False
        Return MathUtility.LinearInterpReal(x, lookup_x, lookup_y, DidInterpolate)
    End Function

    Private Function isBetween(ByVal mth As Integer, ByVal start As Integer, ByVal finish As Integer) As Boolean
        If (start > finish) Then
            Return (mth >= start Or mth <= finish)
        Else
            Return (mth >= start And mth <= finish)
        End If
    End Function

    'Private Sub TestModules()
    '    If (MyPaddock.Fertiliser Is Nothing) Then
    '        Throw New Exception("UrinePatch module requires the presense of fertiliser module in paddock")
    '    End If
    '    If (MyPaddock.Irrigation Is Nothing) Then
    '        Throw New Exception("UrinePatch module requires the presense of irrigation module in paddock")
    '    End If

    'End Sub

End Class