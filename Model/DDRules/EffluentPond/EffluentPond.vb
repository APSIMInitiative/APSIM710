Imports ModelFramework

<Model()> _
Public Class EffluentPond
    Private myExcreta As New Excreta
    Private myWater As Double 'litres

    Public Sub Add(ByVal amount As Excreta)
        myExcreta.Add(amount)
        myWater += amount.N_to_urine / 0.08
    End Sub

    <Description("Remove all effluient from storage")> _
    Public Function Empty() As Excreta
        Dim amount As New Excreta(myExcreta)
        myExcreta.Clear()
        Return amount
    End Function

    <Description("Amount of effluient being held in storage")> _
        <Output()> <Units("TBC")> Public ReadOnly Property Volume() As Double
        Get
            Return myExcreta.N_to_feaces + myExcreta.DM_to_feaces + myExcreta.N_to_urine
        End Get
    End Property
End Class