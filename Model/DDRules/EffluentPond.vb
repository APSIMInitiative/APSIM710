Public Class EffluentPond
        Private myExcreta As New Excreta
        Private myWater As Double 'litres

        Public Sub Add(ByVal amount As Excreta)
                myExcreta.Add(amount)
                myWater += amount.N_to_urine / 0.08
        End Sub

        Public Function Empty() As Excreta
                Dim amount As New Excreta(myExcreta)
                myExcreta.Clear()
                Return amount
        End Function

        Public Function Volume() As Double
                Return myExcreta.N_to_feaces + myExcreta.DM_to_feaces + myExcreta.N_to_urine
        End Function
End Class

Public Class EffluentIrrigator
        Public DebugLevel As Integer = 0

        Public Sub Irrigate(ByVal effluent As EffluentPond, ByVal paddocks As List(Of LocalPaddockType))
                Dim TotalArea As Double = 0
                For Each pdk As LocalPaddockType In paddocks
                        TotalArea += pdk.Area
                Next

                Dim total As Excreta = effluent.Empty
                For Each pdk As LocalPaddockType In paddocks
                        If (DebugLevel > 0) Then
                                Console.Out.WriteLine("      DDRules: Spraying dairy shed effluent to paddock " + pdk.Name)
                        End If
                        Dim proportion As Double = pdk.Area / TotalArea
                        'apply proportion of pond on paddock
                        Dim temp As Excreta
                        temp = total.Multiply(proportion)
                        pdk.UrineApplication(temp.N_to_urine, temp.N_to_urine / 0.08, 0) 'urine N concentration of 8g/l
                        pdk.DungApplication(temp.N_to_feaces, temp.DM_to_feaces)
                Next
        End Sub
End Class