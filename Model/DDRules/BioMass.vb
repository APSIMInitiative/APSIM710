Imports ModelFramework

Public Class BioMass
    Public Name As String
    Public gLeaf, gStem, dLeaf, dStem As Double ' drymatter [kg]
    Private N_concentration As Double = 0 'total N in biomass [kg]
    Public digestibility As Double = 0
    Private myME As Double = 0 'ME value <= 0 then use a calculated value
    Private roundTo As Integer = 1000 'dp

    Public Sub New()
        clear()
    End Sub

    Public Sub New(ByVal other As BioMass)
        Me.Name = other.Name
        Me.gLeaf = other.gLeaf
        Me.gStem = other.gStem
        Me.dLeaf = other.dLeaf
        Me.dStem = other.dStem
        Me.N_concentration = other.N_concentration
        Me.myME = other.myME
        Me.digestibility = other.digestibility
    End Sub

    Public Sub clear()
        Name = "default"
        gLeaf = 0
        gStem = 0
        dLeaf = 0
        dStem = 0
        N_concentration = 0
        digestibility = 0
    End Sub

    Public Function DM_Green() As Double
        Return Math.Round((gLeaf + gStem) * roundTo) / roundTo
    End Function
    Public Function DM_Dead() As Double
        Return Math.Round((dLeaf + dStem) * roundTo) / roundTo
    End Function
    Public Function DM_Total() As Double
        Return DM_Green() + DM_Dead()
    End Function
    Public Function N_Total() As Double
        Return DM_Total() * N_Conc()
    End Function

    Public Property N_Conc() As Double 'N concentration [kgN/kgDM]
        Get
            Return N_concentration
        End Get
        Set(ByVal value As Double)
            If (value > 0) Then
                N_concentration = value
            Else
                N_concentration = 0
            End If
        End Set
    End Property

    Public Function proportionGreenLeaf() As Double
        Return gLeaf / DM_Green()
    End Function
    Public Function proportionDeadLeaf() As Double
        Return dLeaf / DM_Dead()
    End Function
    Public Function proportionDead() As Double
        Return DM_Dead() / DM_Total()
    End Function
    Public Function proportionGreen() As Double
        Return DM_Green() / DM_Total()
    End Function

    Public Overrides Function ToString() As String
        Return Name & " DM = " & DM_Total().ToString("#0") & " @ = " & getME().ToString("#0.0") & "me DM[Green=" & gLeaf.ToString("#0") & ", " & gStem.ToString("#0") & ", Dead=" & dLeaf.ToString("#0") & ", " & dStem.ToString("#0") & "] " & " N = " & N_Total()
    End Function

    Public Function Multiply(ByVal factor As Double) As BioMass
        Dim result As BioMass = New BioMass()
        result.Name = Name
        result.gLeaf = gLeaf * factor
        result.gStem = gStem * factor
        result.dLeaf = dLeaf * factor
        result.dStem = dStem * factor
        result.N_concentration = N_concentration
        result.myME = myME
        result.digestibility = digestibility
        Return result
    End Function

    Public Function Add(ByVal other As BioMass) As BioMass
        Dim t As Double = DM_Total() + other.DM_Total
        If (t <= 0 Or Double.IsInfinity(t) Or Double.IsNaN(t)) Then
            Return New BioMass()
        End If

        Dim result As BioMass = New BioMass()
        result.gLeaf = gLeaf + other.gLeaf
        result.gStem = gStem + other.gStem
        result.dLeaf = dLeaf + other.dLeaf
        result.dStem = dStem + other.dStem
        result.N_concentration = (N_Total() + other.N_Total()) / result.DM_Total()
        If (result.N_concentration.ToString.Contains("NaN")) Then
            Console.WriteLine("DDRules (debug) - " & "BioMass.Add: N_concentration not a number")
            Console.WriteLine("Me = " + ToString())
            Console.WriteLine("Other = " + other.ToString())
            Console.WriteLine("result = " + result.ToString())
        End If

        If (result.DM_Total() > 0) Then
            result.myME = (getME_Total() + other.getME_Total()) / result.DM_Total()
            result.digestibility = (other.digestibility * other.DM_Total() + digestibility * DM_Total()) / result.DM_Total()
        End If
        Return result
    End Function

    Public Function Subtract(ByVal other As BioMass) As BioMass
        Dim result As BioMass = New BioMass()
        result.gLeaf = gLeaf - other.gLeaf
        result.gStem = gStem - other.gStem
        result.dLeaf = dLeaf - other.dLeaf
        result.dStem = dStem - other.dStem
        If (result.DM_Total <> 0) Then
            result.N_concentration = (N_Total() - other.N_Total) / result.DM_Total
        Else
            result.N_Conc = 0
        End If
        If (result.DM_Total() > 0) Then
            result.myME = (getME_Total() - other.getME_Total()) / result.DM_Total()
            result.digestibility = (other.digestibility * other.DM_Total() + digestibility * DM_Total()) / result.DM_Total()
        End If
        Return result
    End Function

    'This convienience function is for use with the SurfaceOM "BiomassRemoved" event
    Public Function toBiomassRemovedType(ByVal fraction_to_residue As Double) As BiomassRemovedType
        Dim result As New BiomassRemovedType
        result.crop_type = "grass"
        result.dm_type = New String() {"grass"}
        result.dlt_crop_dm = New Single() {DM_Total()}  'convert this to g/m^2?
        result.dlt_dm_n = New Single() {N_Total()}      'convert this to g/m^2?
        result.dlt_dm_p = New Single() {0} ' I don't know what to do about P
        result.fraction_to_residue = New Single() {fraction_to_residue}
        Return result
    End Function

    'This convienience function is for use with the "remove_crop_biomass" event
    Public Function toRemoveCropDmType() As RemoveCropBiomassType
        Dim result As New RemoveCropBiomassType
        Dim green As RemoveCropBiomassdmType = getDMType("green", gLeaf, gStem)
        Dim dead As RemoveCropBiomassdmType = getDMType("dead", dLeaf, dStem)
        result.dm = New RemoveCropBiomassdmType() {green, dead}
        Return result
    End Function

    Private Function getDMType(ByVal pool As String, ByVal leaf As Double, ByVal stem As Double) As RemoveCropBiomassdmType
        Dim result As New RemoveCropBiomassdmType
        result.pool = pool
        result.part = New String() {"leaf", "stem"}
        result.dlt = New Double() {leaf / 10, stem / 10} 'convert to g/m^2
        Return result
    End Function

    ' ME pasture calculation by component - values from QGraze defaults
    Private Function calcME() As Double
        Dim l As Double = gLeaf * 13.0 'increased from 12. Not including clover content
        Dim s As Double = gStem * 10.5
        Dim d As Double = DM_Dead() * 9
        Return l + s + d
        'Return 16 * digestibility
    End Function

    Public Sub setME(ByVal PastureME As Double)
        myME = PastureME
    End Sub

    Public Function getME() As Double
        If (DM_Total() > 0) Then
            Return getME_Total() / DM_Total()
        Else
            Return 0
        End If
    End Function


    Public Function getME_Total() As Double
        If (myME <= 0) Then
            Return calcME() 'no ME value set so assume it pasture
        Else
            Return myME * DM_Total()
        End If
    End Function

    Sub [set](ByVal value As Double, ByVal SilageDigestability As Double, ByVal SilageN As Double, ByVal DefualtSilageME As Single)
        gLeaf = value
        gStem = 0
        dLeaf = 0
        dStem = 0
        N_concentration = SilageN
        myME = DefualtSilageME
        digestibility = SilageDigestability
    End Sub

End Class
