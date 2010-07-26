Public Class BioMass
        Public Name As String
        Public gLeaf, gStem, dLeaf, dStem As Double ' drymatter [kg]
        Private N_concentration As Double 'total N in biomass [kg]
        'Private myME As Double = 10 'hard coded ME value. Move to calculated
        Private roundTo As Integer = 1000 'dp

        Public Sub New()
                Me.Name = "default"
                Me.gLeaf = 0
                Me.gStem = 0
                Me.dLeaf = 0
                Me.dStem = 0
                Me.N_concentration = 0
        End Sub

        Public Sub New(ByVal other As BioMass)
                Me.Name = other.Name
                Me.gLeaf = other.gLeaf
                Me.gStem = other.gStem
                Me.dLeaf = other.dLeaf
                Me.dStem = other.dStem
                Me.N_concentration = other.N_concentration
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
                Return DM_Total() * N_concentration
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
                'result.myME = myME
                Return result
        End Function

        Public Function Add(ByVal other As BioMass) As BioMass
                Dim result As BioMass = New BioMass()
                result.gLeaf = gLeaf + other.gLeaf
                result.gStem = gStem + other.gStem
                result.dLeaf = dLeaf + other.dLeaf
                result.dStem = dStem + other.dStem
                result.N_concentration = (N_Total() + other.N_Total) / result.DM_Total
                'If (result.DM_Total() > 0) Then
                '        result.myME = (getME_Total() + other.getME_Total()) / result.DM_Total()
                'End If
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
                'If (result.DM_Total() > 0) Then
                '        result.myME = (getME_Total() + other.getME_Total()) / result.DM_Total()
                'End If
                Return result
        End Function

        Public Function toRemoveCropDmType() As RemoveCropDmType
                Dim result As New RemoveCropDmType
                Dim green As RemoveCropDmdmType = getDMType("green", gLeaf, gStem)
                Dim dead As RemoveCropDmdmType = getDMType("dead", dLeaf, dStem)
                result.dm = New RemoveCropDmdmType() {green, dead} ' including dead mater causes problems with "remove_crop_biomass" call. If green > zero && dead == zero then removal == zero?
                Return result
        End Function

        Private Function getDMType(ByVal pool As String, ByVal leaf As Double, ByVal stem As Double) As RemoveCropDmdmType
                Dim result As New RemoveCropDmdmType
                result.pool = pool
                result.part = New String() {"leaf", "stem"}
                result.dlt = New Double() {leaf / 10, stem / 10} 'convert to g/m^2
                Return result
        End Function

        Public Sub setME(ByVal PastureME As Double)
                'myME = PastureME
        End Sub

        Public Function getME() As Double
                Return getME_Total() / DM_Total() 'myME
        End Function

        Private Function calcME() As Double
                Dim l = gLeaf * 12
                Dim s = gStem * 10.5
                Dim d = DM_Dead() * 9

                Return l + s + d ' myME
        End Function

        Public Function getME_Total() As Double
                Return calcME()
        End Function

        'Public Function getTotal_Nitrogen() As Double
        '    Return myME * DM_Total() * 0.05 'rainss 20100128 - dummy value only
        'End Function
End Class
