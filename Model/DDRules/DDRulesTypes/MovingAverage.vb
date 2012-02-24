Public Class MovingAverage

        Private mySize As Integer
        Private mySum As Double
        Private myData As Queue(Of Double)
        Public Sub New(ByVal NumPeriods As Integer)
                mySize = Math.Max(1, NumPeriods)
                myData = New Queue(Of Double)(mySize)
                mySum = 0
        End Sub

        Public Sub Add(ByVal value As Double)
                If (myData.Count = mySize) Then
                        mySum -= myData.Dequeue()
                End If
                myData.Enqueue(value)
                mySum += value
        End Sub

        Public Function Average() As Double
                If (myData.Count = 0) Then
                        Throw New Exception("MovingAverage does not contain any data points")
                End If
                Return mySum / myData.Count
        End Function
End Class
