Public Module Utils

    Public Function isBetween(ByVal mth As Integer, ByVal start As Integer, ByVal finish As Integer) As Boolean
        If (start > finish) Then
            Return (mth >= start Or mth <= finish)
        Else
            Return (mth >= start And mth <= finish)
        End If
    End Function

    Public Function isBetween(ByVal today As Date, ByVal start As Date, ByVal finish As Date) As Boolean
        Dim a As Date = New Date(today.Year, start.Month, start.Day)
        Dim b As Date = New Date(today.Year, finish.Month, finish.Day)

        If (a > b) Then
            Return (today >= a Or today <= b)
        Else
            Return (today >= a And today <= b)
        End If
    End Function

    Private Function testBetween() As Boolean
        Dim start As Date = Date.Parse("24-apr")
        Dim finish As Date = Date.Parse("14-apr")
        Dim i, j As Integer
        Dim t As Boolean
        For i = 1 To 12
            For j = 1 To 28
                Dim d As Date = New Date(1990, i, j)
                Console.Write(d.ToString(j.ToString() + "/" + i.ToString() + "/1990 = "))
                t = isBetween(d, start, finish)
                Console.WriteLine(t)
            Next j
        Next i
        Return True
    End Function
End Module
