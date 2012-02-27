
<Model()> _
Public Class EffluentIrrigator
    Public DebugLevel As Integer = 0
    <Output()> Public Type As String = "Test Irrigator"

    Public Sub Irrigate(ByVal effluent As EffluentPond, ByVal paddocks As List(Of PaddockWrapper))
        Dim TotalArea As Double = 0
        For Each pdk As PaddockWrapper In paddocks
            TotalArea += pdk.Area
        Next

        Dim total As Excreta = effluent.Empty
        For Each pdk As PaddockWrapper In paddocks
            If (DebugLevel > 0) Then
                Console.Out.WriteLine("      DDRules: Spraying dairy shed effluent to paddock " + pdk.Name)
            End If
            Dim proportion As Double = pdk.Area / TotalArea
            'apply proportion of pond on paddock
            Dim temp As Excreta = total.Multiply(proportion)
            pdk.UrineApplication(temp.N_to_urine, temp.N_to_urine / 0.08, 0) 'urine N concentration of 8g/l
            pdk.DungApplication(temp.N_to_feaces, temp.DM_to_feaces)
        Next
    End Sub
End Class
