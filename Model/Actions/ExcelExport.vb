Public Class ExcelExport
    Public fileList As String

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles bSelectAll.Click
        For i As Integer = 0 To fileListBox.Items.Count - 1
            fileListBox.SetItemChecked(i, True)
        Next
    End Sub

    Private Sub bDeselectAll_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles bDeselectAll.Click
        For i As Integer = 0 To fileListBox.Items.Count - 1
            fileListBox.SetItemChecked(i, False)
        Next
    End Sub

    Private Sub bOK_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles bOK.Click
        Dim allFiles(fileListBox.Items.Count) As String
        fileList = ""
        fileListBox.Items.CopyTo(allFiles, 0)

        For i As Integer = 0 To fileListBox.Items.Count - 1
            If fileListBox.GetItemChecked(i) Then
                fileList += allFiles(i) + ","
            End If
        Next
        If fileList.Length <> 0 Then
            fileList = fileList.Substring(0, fileList.Length - 1)
        End If
        Me.Close()
    End Sub

    Private Sub bCancel_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles bCancel.Click
        Me.Close()
    End Sub
End Class