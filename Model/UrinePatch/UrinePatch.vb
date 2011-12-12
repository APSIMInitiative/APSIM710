Imports CSGeneral
Imports ModelFramework


Public Class UrinePatch

   <Link()> Public MyPaddock As Paddock
   <Link()> Public SoilWater As SoilWat
   <Link()> Public Fertiliser As Fertiliser
   <Link()> Public Irrigation As Irrigation
   <Output()> Public UrineN_FromAnimal As Double            'N in urine from cows in this paddock today, units kgN/ha
   Public AnimalType As String = ""
   <Output()> Public Effective_StockDensity As Double       'stocking density on /ha 24 hour basis, units cows/ha

   <Input()> Private UI_FarmType As String
   '<Input()> Private UI_IrrigSeason As String
    '<Input()> Private UI_StockRate As String
    <Input()> Public UI_BeefPercentage As Double
   Private SUBeefPercentatage As Double

   <Output()> Public Stack_Rain As Double() = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}
   <Output()> Public Stack_AveTemp As Double() = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}
   <Output()> Public Stack_UrineN_FromAnimal As Double() = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}
   <Output()> Public Stack_Effective_StockDensity As Double() = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}
   <Output()> Public Stack_AnimalType As String() = {"None", "None", "None", "None", "None", "None", "None", "None", "None", "None", "None", "None", "None", "None", "None"}

   <Input()> Private month As Integer
   <Input()> Private day As Integer
   <Input()> Private annual_average_rainfall As Single
    <Input()> Private Rain As Double
    <Input()> Private Mint As Double
    <Input()> Private Maxt As Double

   <Output()> Public UrineN_PropLeached As Double = 0.0
   <Output()> Public UrineN_Retained As Double = 0.0
   <Output()> Public UrineN_Leached As Double = 0.0
   <Output()> Public UrineN_Volat As Double = 0.0
   <Output()> Public UrineN_Retained_Patch As Double = 0.0
   <Output()> Public UrineN_Leached_Patch As Double = 0.0
   <Output()> Public UrineConcentration As Double = 0.0
   <Output()> Public UP_AnimalType As String = "None"


   <Input()> Private UI_SoilType As String
   <Input()> Private dlayer As Single()      'mm
   Private SoilDepth As Single = 1500.0 ' mm
   <Output()> Public SoilPAW As Single
   <Output()> Public SoilPAW_fac As Single
   <Output()> Public Animal_fac As Single
   <Output()> Public Amount_fac As Double
   <Output()> Public Rain_fac As Double
   <Output()> Public Day_fac As Double
   <Output()> Public Volat_fac As Double

   Private P1 As Double = 0.107889
   Private P2 As Double = 0.24442
   Private P3 As Double = 0.639043
   Private P4 As Double = 3.217139
   Private P5 As Double = 0.011375
   Private P6 As Double = -0.01538
   Private P7 As Double = 5.102897
   Private P8 As Double = 4.796632
   Private A As Double = 0.0
   Private B As Double = 0.0
   Private C As Double = 0.0
   Private D As Double = 0.0
   <Output()> Public LoadByPAW_fac As Double = 0.0

   Private G1 As Double = -0.38246
   Private G2 As Double = 0.001441
   Private G3 As Double = 17.16542
   Private G4 As Double = 0.000414
   Private G5 As Double = 1.384217
   Private G6 As Double = -0.09355
   Private G7 As Double = -0.0000012
   <Output()> Public Temp2wk_fac As Double = 0.0
   <Output()> Public Temp2wk As Double = 0.0

   Private Q1 As Double = -20.2051
   Private Q2 As Double = -0.03552
   Private Q3 As Double = 0.182615
   Private Q4 As Double = 2610.128
   Private Q5 As Double = -0.40163
   <Output()> Public Rain2wk_fac As Double = 0.0
   <Output()> Public Rain2wk As Double = 0.0


   Private Amount_x As Double() = {0, 200, 400, 600, 800, 1000}
   Private Amount_y As Double() = {0, 0.2, 0.25, 0.4, 0.5, 0.6}

   Private PAW_x As Double() = {0, 40, 175, 500}
   Private PAW_y As Double() = {1.0, 1.0, 0.3, 0.3}

   Private DrainPost2wk_x As Double() = {0, 50, 100, 150, 200, 250}
   Private DrainPost2wk_y As Double() = {0, 0.08, 0.16, 0.24, 0.32, 0.32}  'initially populate with dryland values
   Private DrainPost2wk_irrig_y As Double() = {0, 0, 0.06, 0.13, 0.19, 0.19}
   Private DrainPost2wk_dry_y As Double() = {0, 0.08, 0.16, 0.24, 0.32, 0.32}
   <Output()> Public DrainPost2wk_fac As Double = 0.0

   Private CowsPerHa As Single = 0.0
   Private CowUrinationsPerDay As Single = 10.0
   Private CowAreaPerUrination As Single = 0.66
   Private CowUrineAreaPerHa As Single = 0.0

   Private LambPerHa As Single = 0.0
   Private LambUrinationsPerDay As Single = 30.0
   Private LambAreaPerUrination As Single = 0.1
   Private LambUrineAreaPerHa As Single = 0.0

   Private SheepPerHa As Single = 0.0
   Private SheepUrinationsPerDay As Single = 20.0
   Private SheepAreaPerUrination As Single = 0.15
   Private SheepUrineAreaPerHa As Single = 0.0

   Private BeefPerHa As Single = 0.0
   Private BeefUrinationsPerDay As Single = 10.0
   Private BeefAreaPerUrination As Single = 0.44
   Private BeefUrineAreaPerHa As Single = 0.0

   Private MaxPropLeach As Single = 0.95





   <EventHandler()> Public Sub OnInit2()

      Console.WriteLine("Initialising UrinePatch")

      TestModules()  'ensure that irrigation and fertiliser are in the simulation

      If UI_SoilType.ToLower = "extremelylight" Then
         SoilPAW = 42  'to 150 cm deep
      ElseIf UI_SoilType.ToLower = "verylight" Then
         SoilPAW = 99  'to 150 cm deep
      ElseIf UI_SoilType.ToLower = "light" Then
         SoilPAW = 104  'to 150 cm deep
      ElseIf UI_SoilType.ToLower = "medium" Then
         SoilPAW = 153  'to 150 cm deep
      ElseIf UI_SoilType.ToLower = "heavy" Then
         SoilPAW = 232  'to 150 cm deep
      ElseIf UI_SoilType.ToLower = "poorlydrained" Then
         SoilPAW = 242  'to 150 cm deep
      ElseIf UI_SoilType.ToLower = "poorlydrainedlight" Then
         SoilPAW = 93  'to 150 cm deep
      Else
         Dim sw_dul As Single() = SoilWater.dul_dep           'sw at DUL
         Dim sw_15 As Single() = SoilWater.ll15_dep         'sw at LL15 

         SoilPAW = 0.0
         Dim CumDepth As Single = 0.0
         For z As Integer = 0 To dlayer.Length - 1
            CumDepth += dlayer(z)
            SoilPAW += (sw_dul(z) - sw_15(z)) * Math.Min(1.0, Math.Max(0.0, (1.0 - (CumDepth - SoilDepth) / dlayer(z))))
         Next
      End If
      SoilPAW_fac = LinearInterp(SoilPAW, PAW_x, PAW_y)

      If UI_FarmType.ToLower = "dairy" Then
      ElseIf UI_FarmType.ToLower = "sheepandbeef" Then
      ElseIf UI_FarmType.ToLower = "traditionalarable" Then
      ElseIf UI_FarmType.ToLower = "intensivearable" Then
      Else
         Throw New Exception("Farm type - " & UI_FarmType & " - not allowed")
      End If

      'If UI_IrrigSeason = "None" Then
      '    For i As Integer = 0 To DrainPost2wk_y.Length - 1
      '        DrainPost2wk_y(i) = DrainPost2wk_dry_y(i)
      '    Next
      'ElseIf UI_IrrigSeason = "Short" Then
      '    For i As Integer = 0 To DrainPost2wk_y.Length - 1
      '        DrainPost2wk_y(i) = DrainPost2wk_irrig_y(i)
      '    Next
      'ElseIf UI_IrrigSeason = "Long" Then
      '    For i As Integer = 0 To DrainPost2wk_y.Length - 1
      '        DrainPost2wk_y(i) = DrainPost2wk_irrig_y(i)
      '    Next
      'Else
      '    Throw New Exception("Irrigation type not allowed")
      'End If


      Console.WriteLine("Finished Initialising UrinePatch")

   End Sub

    <EventHandler()> Public Sub OnNewMet(ByVal NewMet As NewMetType)

        If day = 1 Then
            UP_AnimalType = "None"

        End If

        UrineN_FromAnimal = 0.0
        UrineN_PropLeached = 0.0
        UrineN_Retained = 0.0
        UrineN_Leached = 0.0
        UrineN_Leached_Patch = 0.0
        UrineConcentration = 0.0
        Day_fac = 0.0
        Amount_fac = 0.0
        DrainPost2wk_fac = 0.0
        UP_AnimalType = "None"

        LoadByPAW_fac = 0.0
        Temp2wk_fac = 0.0
        Rain2wk_fac = 0.0
        Temp2wk = 0.0
        Rain2wk = 0.0

        For i As Integer = 14 To 1 Step -1
            Stack_Rain(i) = Stack_Rain(i - 1)
            Stack_AveTemp(i) = Stack_AveTemp(i - 1)
            Stack_UrineN_FromAnimal(i) = Stack_UrineN_FromAnimal(i - 1)
            Stack_Effective_StockDensity(i) = Stack_Effective_StockDensity(i - 1)
            Stack_AnimalType(i) = Stack_AnimalType(i - 1)
        Next
        Stack_Rain(1) = Rain
        Stack_AveTemp(1) = (Mint + Maxt) / 2.0

    End Sub


   <EventHandler()> Public Sub OnApplyUrine(ByVal UrineData As ApplyUrineType)   '(ByVal Amount As Double, ByVal StockDensity As Double, ByVal StockType As String)

        UrineN_FromAnimal = UrineData.UrineNLoad
      Effective_StockDensity = UrineData.StockDensity
      AnimalType = UrineData.StockType

      Stack_UrineN_FromAnimal(1) = UrineN_FromAnimal
      Stack_Effective_StockDensity(1) = Effective_StockDensity
      Stack_AnimalType(1) = AnimalType ' will not deal with more than one animal type at this point

      'Console.WriteLine("UrinePatch - Amount = " & UrineN_FromAnimal.ToString)
      'Console.WriteLine("UrinePatch - StockDensity = " & Effective_StockDensity.ToString)
      'Console.WriteLine("UrinePatch - StockType = " & AnimalType)



   End Sub




   <EventHandler()> Public Sub OnProcess()
      If Stack_UrineN_FromAnimal(14) > 0.0 Then
         CalcUrineArea(Stack_UrineN_FromAnimal(14), Stack_Effective_StockDensity(14), Stack_AnimalType(14))
      End If


   End Sub

   Private Sub CalcUrineArea(ByVal Amt_Urine As Double, ByVal StockDensity As Double, ByVal Stock As String)

      UP_AnimalType = Stock.ToLower
      'calculate effective application amount to urine patch area
      If UP_AnimalType = "DairyCow".ToLower Then
         Animal_fac = 1.0
         CowUrineAreaPerHa = StockDensity * CowUrinationsPerDay * CowAreaPerUrination / 10000.0
         UrineConcentration = Amt_Urine / CowUrineAreaPerHa
         UrineApplication(UrineConcentration, CowUrineAreaPerHa)
      ElseIf UP_AnimalType = "Lamb".ToLower Then
         Animal_fac = 0.1
         LambUrineAreaPerHa = StockDensity * LambUrinationsPerDay * LambAreaPerUrination / 10000.0
         UrineConcentration = Amt_Urine / LambUrineAreaPerHa
         UrineApplication(UrineConcentration, LambUrineAreaPerHa)
      ElseIf UP_AnimalType = "Sheep".ToLower Then
         Animal_fac = 0.15
         SheepUrineAreaPerHa = StockDensity * SheepUrinationsPerDay * SheepAreaPerUrination / 10000.0
         UrineConcentration = Amt_Urine / SheepUrineAreaPerHa
         UrineApplication(UrineConcentration, SheepUrineAreaPerHa)
      ElseIf UP_AnimalType = "Beef".ToLower Then
         Animal_fac = 1.0
         BeefUrineAreaPerHa = StockDensity * BeefUrinationsPerDay * BeefAreaPerUrination / 10000.0
         UrineConcentration = Amt_Urine / BeefUrineAreaPerHa
         UrineApplication(UrineConcentration, BeefUrineAreaPerHa)
      ElseIf UP_AnimalType = "SheepBeef".ToLower Then  ' this very FarmSIm special case until generalise this 
         'assume that the urine N is proprtional to the SUBeefPercentage (based on relative intake)
         SUBeefPercentatage = (UI_BeefPercentage * 6.0) / (UI_BeefPercentage * 6.0 + (100 - UI_BeefPercentage)) * 100
         Animal_fac = (1.0 * SUBeefPercentatage + 0.15 * (100 - SUBeefPercentatage)) / 100.0
         Dim Amt_Urine_Beef As Double = Amt_Urine * SUBeefPercentatage / 100.0
         Dim Amt_Urine_Sheep As Double = Amt_Urine - Amt_Urine_Beef



         'decompose the stock desnity into sheep and beef based on head percentage using UI_BeefPercentage
         Dim StockDensity_Beef As Double = StockDensity * UI_BeefPercentage / 100.0
         Dim StockDensity_Sheep As Double = StockDensity - StockDensity_Beef

         Dim UrineConcentration_Beef As Double = 0.0
         Dim UrineN_PropLeached_Beef As Double = 0.0
         Dim UrineN_Leached_Patch_Beef As Double = 0.0
         Dim UrineN_Retained_Patch_Beef As Double = 0.0
         Dim UrineN_Leached_Beef As Double = 0.0
         Dim UrineN_Retained_Beef As Double = 0.0

         Dim UrineConcentration_Sheep As Double = 0.0
         Dim UrineN_PropLeached_Sheep As Double = 0.0
         Dim UrineN_Leached_Patch_Sheep As Double = 0.0
         Dim UrineN_Retained_Patch_Sheep As Double = 0.0
         Dim UrineN_Leached_Sheep As Double = 0.0
         Dim UrineN_Retained_Sheep As Double = 0.0

         BeefUrineAreaPerHa = StockDensity_Beef * BeefUrinationsPerDay * BeefAreaPerUrination / 10000.0
         SheepUrineAreaPerHa = StockDensity_Sheep * SheepUrinationsPerDay * SheepAreaPerUrination / 10000.0

         Dim BeefWeighting As Double = BeefUrineAreaPerHa / (BeefUrineAreaPerHa + SheepUrineAreaPerHa)
         Dim SheepWeighting As Double = 1 - BeefWeighting


         'calculate the urine patch properties and do fate
         If StockDensity_Beef > 0.1 Then
            UrineConcentration_Beef = Amt_Urine_Beef / BeefUrineAreaPerHa
            UrineApplication(UrineConcentration_Beef, BeefUrineAreaPerHa)
            UrineN_PropLeached_Beef = UrineN_PropLeached
            UrineN_Leached_Patch_Beef = UrineN_Leached_Patch
            UrineN_Retained_Patch_Beef = UrineN_Retained_Patch
            UrineN_Leached_Beef = UrineN_Leached
            UrineN_Retained_Beef = UrineN_Retained
         Else
            UrineConcentration_Beef = 0.0
         End If
         If StockDensity_Sheep > 0.1 Then
            UrineConcentration_Sheep = Amt_Urine_Sheep / SheepUrineAreaPerHa
            UrineApplication(UrineConcentration_Sheep, SheepUrineAreaPerHa)
            UrineN_PropLeached_Sheep = UrineN_PropLeached
            UrineN_Leached_Patch_Sheep = UrineN_Leached_Patch
            UrineN_Retained_Patch_Sheep = UrineN_Retained_Patch
            UrineN_Leached_Sheep = UrineN_Leached
            UrineN_Retained_Sheep = UrineN_Retained
         Else
            UrineConcentration_Sheep = 0.0
         End If

         UrineConcentration = UrineConcentration_Beef * BeefWeighting + UrineConcentration_Sheep * SheepWeighting
         UrineN_PropLeached = UrineN_PropLeached_Beef * BeefWeighting + UrineN_PropLeached_Sheep * SheepWeighting
         UrineN_Leached_Patch = UrineN_Leached_Patch_Beef + UrineN_Leached_Patch_Sheep
         UrineN_Retained_Patch = UrineN_Retained_Patch_Beef + UrineN_Retained_Patch_Sheep
         UrineN_Leached = UrineN_Leached_Beef + UrineN_Leached_Sheep
         UrineN_Retained = UrineN_Retained_Beef + UrineN_Retained_Sheep
      Else
         Throw New Exception("Stock type " & Stock & " not allowed")
      End If

   End Sub


   Private Sub UrineApplication(ByVal UrinePatchNitrogenConc As Double, ByVal UrinePatchArea As Double)

      'calculate the factors affecting leaching loss from the urine patch area

      ' not useable   SoilPAW_fac = Math.Exp(SoilPAW * (0.0039 * Math.Log(UrinePatchNitrogenConc) - 0.0343)) * 0.5 ' need to get the PAW right
      'Amount_fac = LinearInterp(UrinePatchNitrogenConc, Amount_x, Amount_y)
      'Day_fac = 0.5 + 0.35 * Math.Sin(2 * Math.PI * (day + 40.2) / 365.25)
      'Rain_fac = -0.1179 + 0.00014 * annual_average_rainfall

      A = P1 * Math.Min(5800.0, UrinePatchNitrogenConc) ^ P2    'Excel testing showed the calculation robust to 5900 but should notget above about 3000 even with triples
      B = P3 * A ^ P4
      C = P5 + (P6 * A ^ 3)
      D = P7 + (P8 * A ^ 2)

      LoadByPAW_fac = B + (A - B) / (1 + (C * SoilPAW) ^ D)
      Day_fac = 0.14809 * Math.Exp(-0.00443 * SoilPAW) * Math.Sin(2 * Math.PI * (day + 37.96) / 365.25)

      Temp2wk = 0.0
      Rain2wk = 0.0
      For i As Integer = 1 To 14
         Rain2wk += Stack_Rain(i)
         Temp2wk += Stack_AveTemp(i) / 14.0
      Next

      Dim Pt1 As Double = 1 / (Q1 + Q2 * SoilPAW)
      Dim Num As Double = (Q3 - 1 / (Q2 + Q1 * SoilPAW)) * Rain2wk
      Dim Den As Double = Q4 * UrinePatchNitrogenConc ^ Q5 + Rain2wk
      Rain2wk_fac = Pt1 + Num / Den

      Dim Y As Double = G1 + G2 * SoilPAW + G3 / SoilPAW
      Dim ratio As Double = G4 + G6 * Y / (1 + G5 * Y)
      Temp2wk_fac = Y + (ratio + G7 * UrinePatchNitrogenConc) * Temp2wk

      Volat_fac = 0.0 '0.15 * (1.25 + Math.Sin(2 * Math.PI * (day + 60.0) / 365.25))

      'Amount_fac = Math.Exp(SoilPAW * (0.0053 * Math.Log(UrinePatchNitrogenConc) - 0.045))  'fromRogerio's document
      'Rain_fac = -0.0285 + 0.0625 / (1 + (annual_average_rainfall / 903.95) ^ (-22.462))

      'calculate proportion leached and distribute to appropriate locations
      UrineN_PropLeached = LoadByPAW_fac + Day_fac + Temp2wk_fac + Rain2wk_fac ' and in rain and temperature when avaialabe + Rain_fac
      UrineN_PropLeached = Math.Min(MaxPropLeach, UrineN_PropLeached)
      UrineN_PropLeached = Math.Max(UrineN_PropLeached, 0.01)
      UrineN_PropLeached = UrineN_PropLeached * Animal_fac

      UrineN_Leached_Patch = UrinePatchNitrogenConc * UrineN_PropLeached
      UrineN_Retained_Patch = UrinePatchNitrogenConc - UrineN_Leached_Patch
      UrineN_Leached = UrineN_Leached_Patch * UrinePatchArea
      UrineN_Volat = (UrineN_Retained_Patch * Volat_fac) * UrinePatchArea
      UrineN_Retained = (UrineN_Retained_Patch * (1.0 - Volat_fac)) * UrinePatchArea

      Dim Default_Application_Depth As Double = 10   ' apply N at a depth of 10mm
        Dim volume As Double = UrinePatchNitrogenConc / 0.08               ' liquid volume of the urine application
        Dim fert As FertiliserApplicationType = New FertiliserApplicationType()
        fert.Amount = UrineN_Retained
        fert.Depth = Default_Application_Depth
        fert.Type = "urea_n"
        Fertiliser.Apply(fert)
      'MyPaddock.Irrigation.Apply(volume)

   End Sub

   Public Function LinearInterp(ByVal x As Single, ByVal lookup_x() As Double, ByVal lookup_y() As Double) As Double
      Dim DidInterpolate As Boolean = False
      Return MathUtility.LinearInterpReal(x, lookup_x, lookup_y, DidInterpolate)
   End Function

   Private Sub TestModules()
      If (Fertiliser Is Nothing) Then
         Throw New Exception("UrinePatch module requires the presense of fertiliser module in paddock")
      End If
      If (Irrigation Is Nothing) Then
         Throw New Exception("UrinePatch module requires the presense of irrigation module in paddock")
      End If

   End Sub

End Class