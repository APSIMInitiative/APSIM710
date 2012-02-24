
Public Class Excreta
    Public N_to_feaces As Double
    Public DM_to_feaces As Double
    Public N_to_urine As Double

    Public Sub New()
        Clear()
    End Sub

    Public Sub New(ByVal other As Excreta)
        N_to_feaces = other.N_to_feaces
        N_to_urine = other.N_to_urine
        DM_to_feaces = other.DM_to_feaces
    End Sub

    Public Sub New(ByVal Nurine As Double, ByVal Ndung As Double, ByVal DMdung As Double)
        N_to_feaces = Nurine
        N_to_urine = Ndung
        DM_to_feaces = DMdung
    End Sub

    Public Sub Clear()
        N_to_feaces = 0
        N_to_urine = 0
        DM_to_feaces = 0
    End Sub

    Public Sub Subtract(ByVal other As Excreta)
        N_to_feaces -= other.N_to_feaces
        N_to_urine -= other.N_to_urine
        DM_to_feaces -= other.DM_to_feaces
    End Sub

    Public Sub Add(ByVal other As Excreta)
        N_to_feaces += other.N_to_feaces
        N_to_urine += other.N_to_urine
        DM_to_feaces += other.DM_to_feaces
    End Sub

    Public Function Multiply(ByVal factor As Double) As Excreta
        Return New Excreta(N_to_feaces * factor, N_to_urine * factor, DM_to_feaces * factor)
    End Function

    Public Overrides Function ToString() As String
        Dim result As String = N_to_feaces.ToString("0.##") + ", "
        result += N_to_urine.ToString("0.##") + ", "
        result += DM_to_feaces.ToString("0.##")
        Return result
    End Function
End Class
