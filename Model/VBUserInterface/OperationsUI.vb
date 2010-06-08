Imports System.Xml
Imports CSGeneral
Imports Controllers


Public Class OperationsUI
    Inherits BaseView


    Overrides Sub OnRefresh()
        StartOfDayGrid.Rows.Clear()
        EndOfDayGrid.Rows.Clear()
        For Each child As XmlNode In XmlHelper.ChildNodes(Data, "operation")
            If XmlHelper.Attribute(child, "condition") = "start_of_day" Then
                Dim RowIndex As Integer = StartOfDayGrid.Rows.Add()
                StartOfDayGrid.Rows(RowIndex).Cells(0).Value = XmlHelper.Value(child, "date")
                StartOfDayGrid.Rows(RowIndex).Cells(1).Value = XmlHelper.Value(child, "action")
            Else
                Dim RowIndex As Integer = EndOfDayGrid.Rows.Add()
                EndOfDayGrid.Rows(RowIndex).Cells(0).Value = XmlHelper.Value(child, "date")
                EndOfDayGrid.Rows(RowIndex).Cells(1).Value = XmlHelper.Value(child, "action")
            End If
        Next
    End Sub

    Public Overrides Sub OnSave()
        Data.RemoveAll()
        Dim Row As Integer = 0
        While Row < StartOfDayGrid.RowCount - 1 And StartOfDayGrid.Rows(Row).Cells(0).Value <> ""
            Dim NewNode As XmlNode = XmlHelper.CreateNode(Data.OwnerDocument, "operation", "")
            XmlHelper.SetAttribute(NewNode, "condition", "start_of_day")
            XmlHelper.SetValue(NewNode, "date", StartOfDayGrid.Rows(Row).Cells(0).Value)
            XmlHelper.SetValue(NewNode, "action", StartOfDayGrid.Rows(Row).Cells(1).Value)
            Data.AppendChild(NewNode)
            Row = Row + 1
        End While
        Row = 0
        While Row < EndOfDayGrid.RowCount - 1 And EndOfDayGrid.Rows(Row).Cells(0).Value <> ""
            Dim NewNode As XmlNode = XmlHelper.CreateNode(Data.OwnerDocument, "operation", "")
            XmlHelper.SetAttribute(NewNode, "condition", "end_of_day")
            XmlHelper.SetValue(NewNode, "date", EndOfDayGrid.Rows(Row).Cells(0).Value)
            XmlHelper.SetValue(NewNode, "action", EndOfDayGrid.Rows(Row).Cells(1).Value)
            Data.AppendChild(NewNode)
            Row = Row + 1
        End While
    End Sub

End Class
