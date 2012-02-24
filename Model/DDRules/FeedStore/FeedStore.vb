Public Class FeedStore
    Private Total As New BioMass
    Private AddedToday As New BioMass
    Private RemovedToday As New BioMass
    Public doStore As Boolean = True

    Public Sub New()
        Total = New BioMass()
        AddedToday = New BioMass()
        RemovedToday = New BioMass()
    End Sub

    Public Sub Prepare()
        'If (Total.getME_Total <= 0) Then
        '        '       Total = New BioMass() 'this is here because of some unusual initilisation problems (undfined N concentration)
        'End If
        AddedToday = New BioMass
        RemovedToday = New BioMass
    End Sub

    'This adds a the passed quantity of biomass onto the silage heap
    'The heap is not implement as a list/queue but this could be a good idea
    Public Sub Add(ByVal feed As BioMass)
        If (feed.DM_Total > 0) Then
            AddedToday = AddedToday.Add(feed)
            If (doStore) Then
                Total = Total.Add(feed)
            End If
        End If
    End Sub

    'This adds a the passed quantity of biomass onto the silage heap
    'The heap is not implement as a list/queue but this could be a good idea
    Public Function Remove(ByVal EnergyRequired As Double) As BioMass
        Dim result As New BioMass
        If (Total.getME_Total <= 0) Then
            Return result 'no feed avalible
        End If
        Dim proportion As Double = EnergyRequired / Total.getME_Total
        If (proportion > 1.0) Then
            proportion = 1.0
        End If
        result = Total.Multiply(proportion)
        Total = Total.Multiply(1 - proportion)
        RemovedToday = RemovedToday.Add(result)
        Return result
    End Function

    'Note: This function removes mass from a heap no matter if the amount is there or not. This is to allow tracking of surchased in feed.
    Public Function Remove(ByVal amount As BioMass) As BioMass
        RemovedToday = RemovedToday.Add(amount)
        Return amount
    End Function

    Public Function DMRemovedToday() As Double
        Return RemovedToday.DM_Total
    End Function

    Public Function DMAddedToday() As Double
        Return AddedToday.DM_Total
    End Function

    Public Function DM() As Double
        Return Total.DM_Total
    End Function

    Public Property MEContent() As Double
        Get
            Return Total.getME
        End Get
        Set(ByVal value As Double)
            Total.setME(value)
        End Set
    End Property

    Public Overrides Function ToString() As String
        Return "/n   Added = " + AddedToday.ToString() + "/n   RemovedToday = " + RemovedToday.ToString()
    End Function
End Class
