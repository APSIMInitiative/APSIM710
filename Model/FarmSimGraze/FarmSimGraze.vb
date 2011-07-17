Imports CSGeneral
Imports ModelFramework


Public Class FarmSimGraze
   Inherits Instance

   'these are to cope with the fact that AgPasture uses different conventions to the other crop modules - IRRITATING
   Public Variable4TotalDM As String = "topstotalwt"
   Public Variable4TotalN As String = "topstotaln"
   Public UnitsMultiplier As Integer = 10.0   'AgPasture works in kg/ha and the others in g/m2 !  NEED TO FIX!!!!!!!!

   <Link()> Public MyPaddock As Paddock

   <Input()> Private UI_FarmType As String
   <Input()> Private UI_BeefPercentage As Single   ' beef percentage on a per head basis
   Private SUBeefPercentage As Single

   <Input()> Private day As Integer
   <Input(True)> Private Crop2Graze As String = "None"
   <Output()> Public FSG_Crop2Graze As String = "None"

   <Output()> Public DM_trigger As Double = 0.0   ' consider changing to rotation rate though
   <Output()> Public DM_residual As Double = 0.0
   Private DM_grazable As Boolean = False  ' got error when tried to report this
   <Output()> Public DryMatter_Test As Double = 0.0
   <Output()> Public DryMatter_PreGrazing As Double = 0.0
   <Output()> Public DryMatter_PostGrazing As Double = 0.0
   <Output()> Public N_PreGrazing As Double = 0.0
   <Output()> Public N_PostGrazing As Double = 0.0
   <Output()> Public DryMatter_Intake As Double = 0.0
   <Output()> Public PropDMRemoval As Double = 0.0
   <Output()> Public N_Intake As Double = 0.0
   <Output()> Public N_Urine As Double = 0.0
   <Output()> Public N_Dung As Double = 0.0
   <Output()> Public N_Product As Double = 0.0
   <Output()> Public AnimalType As String = "None"

   <Output()> Public IntakePerHead As Double = 0.0
   <Output()> Public EffectiveStockDensity As Double = 0.0



   Public GreenLeaf As Double = 0.0
   Public GreenStem As Double = 0.0
   Public DeadLeaf As Double = 0.0
   Public DeadStem As Double = 0.0

   Private DayOfYear_x As Double() = {0, 15, 46, 75, 106, 136, 167, 197, 228, 259, 289, 320, 350, 366}

   Private Intake_y As Double() = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
   Private N2Product_y As Double() = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
   Private N2Dung_y As Double() = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
   Private N2Urine_y As Double() = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}


   Private SheepIntake_y As Double() = {1.5, 1, 1.3, 1.2, 1.2, 1.2, 1.4, 2.1, 2, 2.5, 3, 3, 2.1, 1.5}
   Private SheepN2Product_y As Double() = {0.1, 0.1, 0.1, 0.1, 0.075, 0.075, 0.05, 0.05, 0.05, 0.075, 0.1, 0.1, 0.1, 0.1}
   Private SheepN2Dung_y As Double() = {0.36, 0.36, 0.36, 0.36, 0.37, 0.37, 0.38, 0.38, 0.38, 0.37, 0.36, 0.36, 0.36, 0.36}
   Private SheepN2Urine_y As Double() = {0.54, 0.54, 0.54, 0.54, 0.555, 0.555, 0.57, 0.57, 0.57, 0.555, 0.54, 0.54, 0.54, 0.54}

   Private LambIntake_y As Double() = {1.3, 1.37, 1.37, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.2, 1.3, 1.25, 1.18, 1.3}
   Private LambN2Product_y As Double() = {0.15, 0.15, 0.15, 0.15, 0.125, 0.125, 0.1, 0.1, 0.1, 0.125, 0.15, 0.15, 0.15, 0.15}
   Private LambN2Dung_y As Double() = {0.34, 0.34, 0.34, 0.34, 0.35, 0.35, 0.36, 0.36, 0.36, 0.35, 0.34, 0.34, 0.34, 0.34}
   Private LambN2Urine_y As Double() = {0.51, 0.51, 0.51, 0.51, 0.525, 0.525, 0.54, 0.54, 0.54, 0.525, 0.51, 0.51, 0.51, 0.51}

   Private BeefIntake_y As Double() = {10.1, 9.8, 9.8, 4, 5.7, 6.5, 6.4, 4.6, 5, 6.6, 7.8, 10.3, 10.3, 10.1}
   Private BeefN2Product_y As Double() = {0.2, 0.2, 0.2, 0.2, 0.15, 0.15, 0.1, 0.1, 0.1, 0.15, 0.15, 0.2, 0.2, 0.2}
   Private BeefN2Dung_y As Double() = {0.32, 0.32, 0.32, 0.32, 0.34, 0.34, 0.36, 0.36, 0.36, 0.34, 0.34, 0.32, 0.32, 0.32}
   Private BeefN2Urine_y As Double() = {0.48, 0.48, 0.48, 0.48, 0.51, 0.51, 0.54, 0.54, 0.54, 0.51, 0.51, 0.48, 0.48, 0.48}

   Private DairyCowIntake_y As Double() = {15, 15, 14.5, 13.5, 12.1, 8.5, 6.5, 6.5, 10, 13.5, 15, 15, 15, 15}
   Private DairyCowN2Product_y As Double() = {0.3, 0.3, 0.3, 0.25, 0.2, 0.15, 0.1, 0.1, 0.15, 0.25, 0.3, 0.3, 0.3, 0.36}
   Private DairyCowN2Dung_y As Double() = {0.28, 0.28, 0.28, 0.3, 0.32, 0.34, 0.36, 0.36, 0.34, 0.3, 0.28, 0.28, 0.28, 0.256}
   Private DairyCowN2Urine_y As Double() = {0.42, 0.42, 0.42, 0.45, 0.48, 0.51, 0.54, 0.54, 0.51, 0.45, 0.42, 0.42, 0.42, 0.384}


   Private DayOfYear_ryegrass_clover_x As Double() = {0, 15, 46, 75, 106, 136, 167, 197, 228, 259, 289, 320, 350, 366}
   Private TriggerDM_ryegrass_clover_y As Double() = {2200, 2200, 2200, 2000, 1800, 1800, 1800, 1800, 1900, 2000, 2200, 2200, 2200, 2200}
   Private ResidualDM_ryegrass_clover_y As Double() = {1200, 1200, 1200, 1000, 600, 600, 600, 600, 700, 900, 1200, 1200, 1200, 1200}
   Private TriggerDM_Grassseed_y As Double() = {2200, 2200, 2200, 2000, 1500, 1200, 1200, 1900, 3000, 4000, 4000, 2200, 2200, 2200}
   Private ResidualDM_Grassseed_y As Double() = {1200, 1200, 1200, 1000, 600, 600, 600, 600, 2000, 3000, 3000, 1200, 1200, 1200}
   Private TriggerDM_Wheat_y As Double() = {1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100}
   Private ResidualDM_Wheat_y As Double() = {1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000}
   Private TriggerDM_kale_y As Double() = {2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000}
   Private ResidualDM_kale_y As Double() = {1900, 1900, 1900, 1900, 1900, 1900, 1900, 1900, 1900, 1900, 1900, 1900, 1900, 1900}
   Private TriggerDM_itallianryegrass_y As Double() = {1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100, 1100}
   Private ResidualDM_itallianryegrass_y As Double() = {1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000}



   <EventHandler()> Public Sub OnInit2()

      Console.WriteLine("Initialising FarmSimGraze")

      FSG_Crop2Graze = Crop2Graze

      SUBeefPercentage = (UI_BeefPercentage * 6.0) / (UI_BeefPercentage * 6.0 + (100 - UI_BeefPercentage))
      If UI_FarmType.ToLower = "dairy" Then
         AnimalType = "DairyCow"
         For i As Integer = 0 To 13
            Intake_y(i) = DairyCowIntake_y(i)
            N2Product_y(i) = DairyCowN2Product_y(i)
            N2Dung_y(i) = DairyCowN2Dung_y(i)
            N2Urine_y(i) = DairyCowN2Urine_y(i)
         Next
      ElseIf UI_FarmType.ToLower = "sheepandbeef" Then
         AnimalType = "SheepBeef"
         Dim PropBeef As Double = UI_BeefPercentage / 100.0
         Dim PropSheep As Double = 1.0 - PropBeef
         For i As Integer = 0 To 13
            Intake_y(i) = (BeefIntake_y(i) * PropBeef) + (SheepIntake_y(i) * PropSheep)
            N2Product_y(i) = (BeefN2Product_y(i) * PropBeef) + (SheepN2Product_y(i) * PropSheep)
            N2Dung_y(i) = (BeefN2Dung_y(i) * PropBeef) + (SheepN2Dung_y(i) * PropSheep)
            N2Urine_y(i) = (BeefN2Urine_y(i) * PropBeef) + (SheepN2Urine_y(i) * PropSheep)
         Next
      ElseIf UI_FarmType.ToLower = "intensivearable" Then
         AnimalType = "Lamb"
         For i As Integer = 0 To 13
            Intake_y(i) = LambIntake_y(i)
            N2Product_y(i) = LambN2Product_y(i)
            N2Dung_y(i) = LambN2Dung_y(i)
            N2Urine_y(i) = LambN2Urine_y(i)
         Next
      ElseIf UI_FarmType.ToLower = "traditionalarable" Then
         AnimalType = "Sheep"
         For i As Integer = 0 To 13
            Intake_y(i) = SheepIntake_y(i)
            N2Product_y(i) = SheepN2Product_y(i)
            N2Dung_y(i) = SheepN2Dung_y(i)
            N2Urine_y(i) = SheepN2Urine_y(i)
         Next
      Else
         Throw New Exception("Farm type - " & UI_FarmType & " - not allowed")
      End If
      Console.WriteLine("Finished Initialising FarmSimGraze")

   End Sub

   <EventHandler()> Public Sub OnPrepare()

      FSG_Crop2Graze = Crop2Graze

      EffectiveStockDensity = 0.0
      DryMatter_Test = 0.0
      DryMatter_PreGrazing = 0.0
      DryMatter_PostGrazing = 0.0
      N_PreGrazing = 0.0
      N_PostGrazing = 0.0
      DryMatter_Intake = 0.0
      PropDMRemoval = 0.0
      N_Intake = 0.0
      N_Urine = 0.0
      N_Dung = 0.0
      N_Product = 0.0
      IntakePerHead = 0.0


      If Crop2Graze.ToLower = "none" Then
         DM_trigger = 0.0
         DM_residual = 0.0
      ElseIf Crop2Graze.ToLower = "ryegrass_clover" Then
         DM_trigger = LinearInterp(day, DayOfYear_x, TriggerDM_ryegrass_clover_y)
         DM_residual = LinearInterp(day, DayOfYear_x, ResidualDM_ryegrass_clover_y)
         Variable4TotalDM = "AboveGroundWt"
         Variable4TotalN = "AboveGroundN"
         UnitsMultiplier = 1
      ElseIf Crop2Graze.ToLower = "grassseed" Then
         DM_trigger = LinearInterp(day, DayOfYear_x, TriggerDM_Grassseed_y)
         DM_residual = LinearInterp(day, DayOfYear_x, ResidualDM_Grassseed_y)
         Variable4TotalDM = "topstotalwt"
         Variable4TotalN = "topstotaln"
         UnitsMultiplier = 10.0
      ElseIf Crop2Graze.ToLower = "wheat" Then
         DM_trigger = LinearInterp(day, DayOfYear_x, TriggerDM_Wheat_y)
         DM_residual = LinearInterp(day, DayOfYear_x, ResidualDM_Wheat_y)
         Variable4TotalDM = "topstotalwt"
         Variable4TotalN = "topstotaln"
         UnitsMultiplier = 10.0
      ElseIf Crop2Graze.ToLower = "kale" Then
         DM_trigger = LinearInterp(day, DayOfYear_x, TriggerDM_kale_y)
         DM_residual = LinearInterp(day, DayOfYear_x, ResidualDM_kale_y)
         Variable4TotalDM = "topstotalwt"
         Variable4TotalN = "topstotaln"
         UnitsMultiplier = 10.0
      ElseIf Crop2Graze.ToLower = "itallianryegrass" Then
         DM_trigger = LinearInterp(day, DayOfYear_x, TriggerDM_itallianryegrass_y)
         DM_residual = LinearInterp(day, DayOfYear_x, ResidualDM_itallianryegrass_y)
         Variable4TotalDM = "topstotalwt"
         Variable4TotalN = "topstotaln"
         UnitsMultiplier = 10.0
      Else
         Throw New Exception("FarmSimGraze is not set up for " & Crop2Graze)
      End If


      'find amount of DM in paddock and graze if at trigger
      If Crop2Graze.ToLower = "none" Then
         'do nothing
         DM_grazable = False
      Else
         DryMatter_Test = GetCropDM(Crop2Graze, Variable4TotalDM, UnitsMultiplier)

         If DryMatter_Test >= DM_trigger Then
            DM_grazable = True
            DryMatter_PreGrazing = GetCropDM(Crop2Graze, Variable4TotalDM, UnitsMultiplier)
            N_PreGrazing = GetCropN(Crop2Graze, Variable4TotalN, UnitsMultiplier)

            PropDMRemoval = (DryMatter_PreGrazing - DM_residual) / DryMatter_PreGrazing

            GrazeDM(PropDMRemoval, Crop2Graze)

            DryMatter_PostGrazing = GetCropDM(Crop2Graze, Variable4TotalDM, UnitsMultiplier)

            N_PostGrazing = GetCropN(Crop2Graze, Variable4TotalN, UnitsMultiplier)
            N_Intake = N_PreGrazing - N_PostGrazing
            DryMatter_Intake = DryMatter_PreGrazing - DryMatter_PostGrazing

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
         DungApplication(N_Dung, N_Dung / 0.025) ' Hanyes and Williams 1993 - Dung generally 2-2.8% N on DM basis
      Else
         IntakePerHead = 0.0
         EffectiveStockDensity = 0.0
         N_Urine = 0.0
         N_Dung = 0.0
         N_Product = 0.0
      End If

   End Sub


   Private Function GetCropDM(ByVal Crop2Graze, ByVal Variable4TotalDM, ByVal UnitsMultiplier)

      Return (MyPaddock.ComponentByName(Crop2Graze).Variable(Variable4TotalDM).ToDouble * UnitsMultiplier)

   End Function

   Private Function GetCropN(ByVal Crop2Graze, ByVal Variable4TotalN, ByVal UnitsMultiplier)

      Return (MyPaddock.ComponentByName(Crop2Graze).Variable(Variable4TotalN).ToDouble * UnitsMultiplier)

   End Function


   Public Sub GrazeDM(ByVal PropDMRemoval As Double, ByVal Crop2Graze As String)

      'Dim GZ As GrazeType = New GrazeType()
      'GZ.amount = DryMatter_Intake
      'GZ.type = "removal"
      'GZ.sender = "FarmSimGraze"
      'MyPaddock.Publish("graze", GZ)

      Dim GreenLeaf As Double = 0.0
      Dim GreenStem As Double = 0.0
      Dim DeadLeaf As Double = 0.0
      Dim DeadStem As Double = 0.0
      Dim dlt_GreenLeaf As Double = 0.0
      Dim dlt_GreenStem As Double = 0.0
      Dim dlt_DeadLeaf As Double = 0.0
      Dim dlt_DeadStem As Double = 0.0

      GreenLeaf = MyPaddock.ComponentByName(Crop2Graze).Variable("leafgreenwt").ToDouble
      GreenStem = MyPaddock.ComponentByName(Crop2Graze).Variable("stemgreenwt").ToDouble
      DeadLeaf = MyPaddock.ComponentByName(Crop2Graze).Variable("leafsenescedwt").ToDouble
      DeadStem = MyPaddock.ComponentByName(Crop2Graze).Variable("stemsenescedwt").ToDouble

      dlt_GreenLeaf = PropDMRemoval * GreenLeaf
      dlt_GreenStem = PropDMRemoval * GreenStem
      dlt_DeadLeaf = PropDMRemoval * DeadLeaf
      dlt_DeadStem = PropDMRemoval * DeadStem


        Dim GreenRemoveCropDmDm As New RemoveCropBiomassdmType
      GreenRemoveCropDmDm.pool = "green"
      GreenRemoveCropDmDm.part = New String() {"leaf", "stem"}
      GreenRemoveCropDmDm.dlt = New Double() {dlt_GreenLeaf, dlt_GreenStem}
        Dim MyRemoveCropDM As New RemoveCropBiomassType
        Dim DeadRemoveCropDmDm As New RemoveCropBiomassdmType
      If Crop2Graze.ToLower = "ryegrass_clover" Then
         DeadRemoveCropDmDm.pool = "dead"
      Else
         DeadRemoveCropDmDm.pool = "senesced"
      End If

      If (dlt_DeadLeaf + dlt_DeadStem) > 0.0 Then
         If (dlt_DeadStem) <= 0.0001 Then  'implicitly asumming that if there will not be a possibility of dead stem without dead leaf
            DeadRemoveCropDmDm.part = New String() {"leaf"}
            DeadRemoveCropDmDm.dlt = New Double() {dlt_DeadLeaf}
                MyRemoveCropDM.dm = New RemoveCropBiomassdmType() {GreenRemoveCropDmDm, DeadRemoveCropDmDm}
         Else
            DeadRemoveCropDmDm.part = New String() {"leaf", "stem"}
            DeadRemoveCropDmDm.dlt = New Double() {dlt_DeadLeaf, dlt_DeadStem}
                MyRemoveCropDM.dm = New RemoveCropBiomassdmType() {GreenRemoveCropDmDm, DeadRemoveCropDmDm}
         End If
      Else
            MyRemoveCropDM.dm = New RemoveCropBiomassdmType() {GreenRemoveCropDmDm}
      End If

      MyPaddock.ComponentByName(Crop2Graze).Publish("remove_crop_biomass", MyRemoveCropDM)



   End Sub


   Private Sub UrineApplication(ByVal UrineN As Double, ByVal StockDensity As Double, ByVal StockType As String)
      Dim ApplyUrineData As New ApplyUrineType
        ApplyUrineData.UrineNLoad = N_Urine
      ApplyUrineData.StockDensity = EffectiveStockDensity
      ApplyUrineData.StockType = AnimalType
      MyPaddock.Publish("ApplyUrine", ApplyUrineData)

   End Sub


   Private Sub DungApplication(ByVal DungN As Double, ByVal DungDM As Double)

      Dim DungData As BiomassRemovedType = New BiomassRemovedType()

      DungData.crop_type = "RuminantDung_PastureFed"
      DungData.dm_type = New String() {"RuminantDung_PastureFed"}
      DungData.dlt_crop_dm = New Single() {DungN + DungDM}
      DungData.dlt_dm_n = New Single() {DungN}
      DungData.dlt_dm_p = New Single() {DungDM * (5.5 / 256.0)} 'Source: McDowell and Stewart (2005) Phosphorus in Fresh and Dry Dung of Grazing Dairy Cattle, Deer, and Sheep, J. Environ. Qual. 34:598-607 (2005). Table 1.
      DungData.fraction_to_residue = New Single() {1.0}

      Dim SOM As Component = MyPaddock.ComponentByType("surfaceom")
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