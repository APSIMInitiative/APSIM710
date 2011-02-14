Imports System.Xml
Imports CSGeneral
Imports Controllers


Public Class OperationsUI
    Inherits BaseView

   ''' <summary>
   ''' Refresh the UI
   ''' </summary>
   Overrides Sub OnRefresh()
      Dim StartDayTable As New DataTable()
      StartDayTable.Columns.Add("Date", GetType(String))
      StartDayTable.Columns.Add("Action", GetType(String))

      Dim EndDayTable As New DataTable()
      EndDayTable.Columns.Add("Date", GetType(String))
      EndDayTable.Columns.Add("Action", GetType(String))

      For Each Child As XmlNode In XmlHelper.ChildNodes(Data, "operation")

         If XmlHelper.Attribute(Child, "condition") = "start_of_day" Then
            Dim NewRow As DataRow = StartDayTable.NewRow()
            StartDayTable.Rows.Add(NewRow)
            NewRow(0) = XmlHelper.Value(Child, "date")
            NewRow(1) = XmlHelper.Value(Child, "action")
         Else
            Dim NewRow As DataRow = EndDayTable.NewRow()
            EndDayTable.Rows.Add(NewRow)
            NewRow(0) = XmlHelper.Value(Child, "date")
            NewRow(1) = XmlHelper.Value(Child, "action")
         End If
      Next
      StartOfDayGrid.DataSourceTable = StartDayTable
      EndOfDayGrid.DataSourceTable = EndDayTable
      StartOfDayGrid.AllowUserToAddRows = True
      EndOfDayGrid.AllowUserToAddRows = True

      ' Size the grid columns sensibly
      StartOfDayGrid.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.None
      StartOfDayGrid.Columns(0).Width = StartOfDayGrid.Columns(0).GetPreferredWidth(DataGridViewAutoSizeColumnMode.AllCells, True) * 2
      StartOfDayGrid.Columns(0).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
      StartOfDayGrid.Columns(1).AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill
      StartOfDayGrid.Columns(1).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft
      EndOfDayGrid.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.None
      EndOfDayGrid.Columns(0).Width = EndOfDayGrid.Columns(0).GetPreferredWidth(DataGridViewAutoSizeColumnMode.AllCells, True) * 2
      EndOfDayGrid.Columns(0).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
      EndOfDayGrid.Columns(1).AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill
      EndOfDayGrid.Columns(1).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft

   End Sub

   ''' <summary>
   ''' Save the contents of the UI
   ''' </summary>
   Public Overrides Sub OnSave()
      Data.RemoveAll()
      ' Add new child nodes.
      For Each Row As DataRow In StartOfDayGrid.DataSourceTable.Rows
         Dim NewNode As XmlNode = XmlHelper.CreateNode(Data.OwnerDocument, "operation", "")
         XmlHelper.SetAttribute(NewNode, "condition", "start_of_day")
         XmlHelper.SetValue(NewNode, "date", Row(0).ToString())
         XmlHelper.SetValue(NewNode, "action", Row(1).ToString())
         Data.AppendChild(NewNode)
      Next
      For Each Row As DataRow In EndOfDayGrid.DataSourceTable.Rows
         Dim NewNode As XmlNode = XmlHelper.CreateNode(Data.OwnerDocument, "operation", "")
         XmlHelper.SetAttribute(NewNode, "condition", "end_of_day")
         XmlHelper.SetValue(NewNode, "date", Row(0).ToString())
         XmlHelper.SetValue(NewNode, "action", Row(1).ToString())
         Data.AppendChild(NewNode)
      Next
   End Sub

End Class
