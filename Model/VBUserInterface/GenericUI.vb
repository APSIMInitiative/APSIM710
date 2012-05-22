Imports System.Collections
Imports System.Collections.Generic
Imports System.Collections.Specialized
Imports System.IO
Imports System.Xml
Imports ApsimFile
Imports Controllers
Imports CSGeneral
Imports UIUtility   'GridUtility.cs , CheckedListBoxCellType.cs
Imports System.Globalization



Public Class GenericUI
   Inherits BaseView

   Private components As System.ComponentModel.IContainer
   Friend WithEvents PictureBox As System.Windows.Forms.PictureBox

   Private EditModeItem As ToolStripMenuItem
   Friend WithEvents Grid As UIBits.EnhancedGrid
   Private IsDirty As Boolean
   Private TypeComboItems As String() = {"text", "date", "yesno", "crop", "cultivars", "classes", "modulename", _
                                         "list", "multilist", "category", "filename", "multiedit"}
   Private YesNoItems As String() = {"yes", "no"}
   Friend WithEvents TextBox As System.Windows.Forms.TextBox
   Friend WithEvents Splitter As System.Windows.Forms.Splitter
   Private InRefresh As Boolean
   Private MemoComponent As Component

   ''' <summary>
   ''' Constructor
   ''' </summary>
   Public Sub New()
      MyBase.New()
      InitializeComponent()

      EditModeItem = New ToolStripMenuItem("Edit mode")
      Grid.PopupMenu.Items.Insert(0, EditModeItem)
      Grid.PopupMenu.Items.Insert(1, New ToolStripSeparator())
      AddHandler EditModeItem.Click, AddressOf OnEditModeClick
      AddHandler Grid.AddingNewRowEvent, AddressOf OnAddingNewRow

   End Sub

   ''' <summary>
   ''' Form overrides dispose to clean up the component list.
   ''' </summary>
   Protected Overloads Overrides Sub Dispose(ByVal disposing As Boolean)
      If disposing Then
         If Not (components Is Nothing) Then
            components.Dispose()
         End If
      End If
      MyBase.Dispose(disposing)
   End Sub

   ''' <summary>
   ''' NOTE: The following procedure is required by the Windows Form Designer
   ''' It can be modified using the Windows Form Designer.  
   ''' Do not modify it using the code editor.
   ''' </summary>
   <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
      Me.PictureBox = New System.Windows.Forms.PictureBox()
      Me.Grid = New UIBits.EnhancedGrid()
      Me.TextBox = New System.Windows.Forms.TextBox()
      Me.Splitter = New System.Windows.Forms.Splitter()
      CType(Me.PictureBox, System.ComponentModel.ISupportInitialize).BeginInit()
      CType(Me.Grid, System.ComponentModel.ISupportInitialize).BeginInit()
      Me.SuspendLayout()
      '
      'MyHelpLabel
      '
      Me.MyHelpLabel.Size = New System.Drawing.Size(1022, 16)
      '
      'PictureBox
      '
      Me.PictureBox.BackgroundImageLayout = System.Windows.Forms.ImageLayout.None
      Me.PictureBox.Dock = System.Windows.Forms.DockStyle.Left
      Me.PictureBox.Location = New System.Drawing.Point(0, 16)
      Me.PictureBox.Name = "PictureBox"
      Me.PictureBox.Size = New System.Drawing.Size(125, 701)
      Me.PictureBox.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize
      Me.PictureBox.TabIndex = 3
      Me.PictureBox.TabStop = False
      '
      'Grid
      '
      Me.Grid.AllowUserToAddRows = False
      Me.Grid.AllowUserToResizeRows = False
      Me.Grid.BackgroundColor = System.Drawing.SystemColors.Window
      Me.Grid.BorderStyle = System.Windows.Forms.BorderStyle.None
      Me.Grid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
      Me.Grid.DataSourceTable = Nothing
      Me.Grid.Dock = System.Windows.Forms.DockStyle.Fill
      Me.Grid.Location = New System.Drawing.Point(125, 141)
      Me.Grid.Name = "Grid"
      Me.Grid.RowHeadersVisible = False
      Me.Grid.Size = New System.Drawing.Size(897, 576)
      Me.Grid.TabIndex = 4
      '
      'TextBox
      '
      Me.TextBox.Dock = System.Windows.Forms.DockStyle.Top
      Me.TextBox.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
      Me.TextBox.Location = New System.Drawing.Point(125, 16)
      Me.TextBox.Multiline = True
      Me.TextBox.Name = "TextBox"
      Me.TextBox.Size = New System.Drawing.Size(897, 125)
      Me.TextBox.TabIndex = 5
      '
      'Splitter
      '
      Me.Splitter.Dock = System.Windows.Forms.DockStyle.Top
      Me.Splitter.Location = New System.Drawing.Point(125, 141)
      Me.Splitter.Name = "Splitter"
      Me.Splitter.Size = New System.Drawing.Size(897, 3)
      Me.Splitter.TabIndex = 6
      Me.Splitter.TabStop = False
      '
      'GenericUI
      '
      Me.Controls.Add(Me.Splitter)
      Me.Controls.Add(Me.Grid)
      Me.Controls.Add(Me.TextBox)
      Me.Controls.Add(Me.PictureBox)
      Me.Name = "GenericUI"
      Me.Size = New System.Drawing.Size(1022, 717)
      Me.Controls.SetChildIndex(Me.MyHelpLabel, 0)
      Me.Controls.SetChildIndex(Me.PictureBox, 0)
      Me.Controls.SetChildIndex(Me.TextBox, 0)
      Me.Controls.SetChildIndex(Me.Grid, 0)
      Me.Controls.SetChildIndex(Me.Splitter, 0)
      CType(Me.PictureBox, System.ComponentModel.ISupportInitialize).EndInit()
      CType(Me.Grid, System.ComponentModel.ISupportInitialize).EndInit()
      Me.ResumeLayout(False)
      Me.PerformLayout()

   End Sub

   ''' <summary>
   ''' Form is being loaded - initialise ourselves.
   ''' </summary>
   Protected Overrides Sub OnLoad()
      MyBase.OnLoad()
   End Sub

   ''' <summary>
   ''' Form is being refreshed.
   ''' </summary>
   Public Overrides Sub OnRefresh()
      InRefresh = True
      Dim Comp As ApsimFile.Component = Controller.ApsimData.Find(NodePath)

      ' set the banner image correctly.
      Me.PictureBox.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Normal

      Dim imagefile As String = Types.Instance.MetaData(Comp.Type, "image")
      If imagefile <> "" And File.Exists(imagefile) Then
         PictureBox.Image = Drawing.Image.FromFile(imagefile)
         PictureBox.Visible = True
      Else
         PictureBox.Visible = False
      End If
      Me.PictureBox.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize

      ' Look for a memo control and enable the TextBox if it exists.

      MemoComponent = Comp.Find("Memo")
      TextBox.Visible = Not IsNothing(MemoComponent)
      Splitter.Visible = Not IsNothing(MemoComponent)
      If Not IsNothing(MemoComponent) Then
         Dim Doc As New XmlDocument()
         Doc.LoadXml(MemoComponent.Contents)
         TextBox.Text = Doc.DocumentElement.InnerText
      End If

      ' Create a DataTable from our data.
      Dim Table As DataTable = CreateTable()

      ' Give the DataTable to our grid.
      Grid.DataSourceTable = Table

      ' Size the grid columns sensibly
      Grid.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.None
      For Col As Integer = 0 To Table.Columns.Count - 1
         Grid.Columns(Col).Width = Grid.Columns(Col).GetPreferredWidth(DataGridViewAutoSizeColumnMode.AllCells, True)
         Grid.Columns(Col).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft
      Next
      Grid.Columns(4).AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill

      IsDirty = False
      GotoEditMode(False)
      InRefresh = False
   End Sub

   ''' <summary>
   ''' Go read all properties from the XML and put into a data table. Returns the
   ''' DataTable to caller.
   ''' </summary>
   Public Function CreateTable() As DataTable

      Dim Table As New DataTable()
      Table.Columns.Add("Name", GetType(String))
      Table.Columns.Add("Type", GetType(String))
      Table.Columns.Add("List items (CSV)", GetType(String))
      Table.Columns.Add("Description", GetType(String))
      Table.Columns.Add("Value", GetType(String))

        For Each Child As XmlNode In Data.ChildNodes
            If Child.ChildNodes.Count <= 1 AndAlso Child.Name <> "TeeChartFormat" AndAlso Child.Name <> "XY" AndAlso _
               Child.Name.ToLower() <> "layer" Then
                Dim NewRow As DataRow = Table.NewRow()
                Table.Rows.Add(NewRow)
                If Child.Name = "category" Then
                    NewRow(0) = "category"
                    NewRow(1) = "category"
                    NewRow(3) = XmlHelper.Attribute(Child, "description")
                Else
                    NewRow(0) = Child.Name
                    If XmlHelper.Attribute(Child, "type") = "" Then
                        NewRow(1) = "text"
                    Else
                        NewRow(1) = XmlHelper.Attribute(Child, "type")
                    End If
                    NewRow(2) = XmlHelper.Attribute(Child, "listvalues")
                    If XmlHelper.Attribute(Child, "description") = "" Then
                        NewRow(3) = XmlHelper.Name(Child)
                    Else
                        NewRow(3) = XmlHelper.Attribute(Child, "description")
                    End If

                    NewRow(4) = Child.InnerText
                End If
            End If
        Next
      CreateTable = Table
   End Function

   ''' <summary>
   ''' Save all our changes back to Data
   ''' </summary>
   Public Overrides Sub OnSave()
      If Grid.IsCurrentCellInEditMode Then
         Grid.EndEditMode()
      End If
      If IsDirty Then
         ' Remove existing XML nodes that don't have any children.
         For i As Integer = Data.ChildNodes.Count - 1 To 0 Step -1
            If Data.ChildNodes(i).ChildNodes.Count <= 1 Then
               Data.RemoveChild(Data.ChildNodes(i))
            End If
         Next

         ' Add new child nodes.
         Dim RowIndex As Integer = 0
         For Each Row As DataRow In Grid.DataSourceTable.Rows
            If Data.Name.ToLower() = "soil" And RowIndex = 0 Then
            Else
                    If Row("Name").ToString() <> "" Then
                        If (Row("Name").ToString() = "#text") Then
                            Data.InnerText = Row("Value").ToString()

                        Else
                            Dim NewProperty As XmlNode = Data.OwnerDocument.CreateElement(Row("Name").ToString())
                            XmlHelper.SetAttribute(NewProperty, "type", Row("Type").ToString())
                            If Row("List items (CSV)").ToString() <> "" Then
                                XmlHelper.SetAttribute(NewProperty, "listvalues", Row("List items (CSV)").ToString())
                            End If
                            If Row("Description").ToString() <> "" Then
                                XmlHelper.SetAttribute(NewProperty, "description", Row("Description").ToString())
                            End If
                            If Row("Value").ToString() <> "" Then
                                ' Make sure we save date rows as dd/mm/yyyy.
                                If Not IsDBNull(Row("Type")) AndAlso Row("Type") = "date" Then
                                    ' Try converting to date.
                                    Try
                                        Dim DateValue As DateTime = Row("Value").ToString()
                                        NewProperty.InnerText = DateValue.ToString("dd/MM/yyyy")
                                    Catch ex As Exception
                                        MessageBox.Show("Cannot convert string: " + Row("Value").ToString() + " to a valid date", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                                        NewProperty.InnerText = Row("Value").ToString()

                                    End Try
                                Else
                                    NewProperty.InnerText = Row("Value").ToString()
                                End If
                            End If
                            Data.AppendChild(NewProperty)
                        End If
                    End If
                End If
                RowIndex = RowIndex + 1
            Next

         ' Save memo text if necessary
         If TextBox.Visible Then
            MemoComponent.Contents = "<Memo>" + TextBox.Text + "</Memo>"
         End If
      End If
   End Sub

   ''' <summary>
   ''' The grid is adding a new row. Format the row the way we want it.
   ''' </summary>
   Public Sub OnAddingNewRow(ByVal NewRow As DataGridViewRow)
      ' Setup the Type combo box.
      Grid.CreateComboInCell(NewRow.Cells(1), TypeComboItems)

      ' Create a list items edit box that can handle lots of chars.
      Dim ListItemTextBox As DataGridViewTextBoxCell = NewRow.Cells(2)
      ListItemTextBox.MaxInputLength = 5000

      ' Now create the correct type of cell in the value field for this row.
      If Not IsDBNull(NewRow.Cells(1).Value) Then
         Select Case NewRow.Cells(1).Value

            Case "yesno"
               Grid.CreateComboInCell(NewRow.Cells(4), YesNoItems)

            Case "list", "multilist"
               If Not IsNothing(NewRow.Cells(2).Value) Then
                  Grid.CreateComboInCell(NewRow.Cells(4), NewRow.Cells(2).Value.ToString().Split(","))
               End If

            Case "multiedit"
               Dim Text As DataGridViewTextBoxCell = NewRow.Cells(4)
               Text.MaxInputLength = 5000
               Text.Style.WrapMode = DataGridViewTriState.True
               NewRow.Height = 80
               
            Case "filename"
               Grid.CreateButtonInCell(NewRow.Cells(4))

            Case "category"
               NewRow.Cells(3).ReadOnly = Not EditModeItem.Checked  ' Make listitems field readonly
               NewRow.Cells(4).ReadOnly = True  ' Make description field readonly
               NewRow.DefaultCellStyle.BackColor = System.Drawing.Color.LightSteelBlue

            Case "modulename"
               Dim Paddock As ApsimFile.Component = Controller.ApsimData.Find(NodePath).FindContainingPaddock()
               While Not IsNothing(Paddock) AndAlso Paddock.Type.ToLower = "folder"
                  Paddock = Paddock.Parent
               End While
               If Not IsNothing(Paddock) Then
                  Grid.CreateComboInCell(NewRow.Cells(4), Paddock.ChildNames)
               End If

            Case "crop"
               Dim Paddock As ApsimFile.Component = Controller.ApsimData.Find(NodePath).FindContainingPaddock()
               While Not IsNothing(Paddock) AndAlso Paddock.Type.ToLower = "folder"
                  Paddock = Paddock.Parent
               End While
               If Not IsNothing(Paddock) Then
                  Dim Crops As New List(Of String)
                  For Each Child As Component In Paddock.ChildNodes
                     If Types.Instance.MetaData(Child.Type, "IsCrop").ToLower = "yes" Then
                        Crops.Add(Child.Name)
                     End If
                  Next
                  Dim CropNames(Crops.Count - 1) As String
                  Crops.CopyTo(CropNames)
                  Grid.CreateComboInCell(NewRow.Cells(4), CropNames)
               End If

            Case "cultivars"
               ' Try and locate a row with crop as the name.
               Dim CropRow As Integer
               For CropRow = 0 To Grid.RowCount - 1
                  If Not IsNothing(Grid.Rows(CropRow).Cells(0).Value) AndAlso Grid.Rows(CropRow).Cells(0).Value.ToLower = "crop" Then
                     Exit For
                  End If
               Next
               ' If we found a crop row then go and get all cultivars for that crop.
               If CropRow < Grid.RowCount AndAlso Not IsDBNull(Grid.Rows(CropRow).Cells(4).Value) Then
                  Dim CropName As String = Grid.Rows(CropRow).Cells(4).Value
                  Dim CropComponent As Component = Controller.ApsimData.Find(NodePath).FindComponentInPaddock(Controller.ApsimData.Find(NodePath), CropName)
                  If Not IsNothing(CropComponent) Then
                     CropName = CropComponent.Type
                  End If
                  Dim Cultivars() As String = Types.Instance.Cultivars(CropName)
                  Grid.CreateComboInCell(NewRow.Cells(4), Cultivars)
               End If

            Case "classes"
               ' Try and locate a row with crop as the name.
               Dim CropRow As Integer
               For CropRow = 0 To Grid.RowCount - 1
                  If Not IsNothing(Grid.Rows(CropRow).Cells(0).Value) AndAlso Grid.Rows(CropRow).Cells(0).Value.ToLower = "crop" Then
                     Exit For
                  End If
               Next
               ' If we found a crop row then go and get all cultivars for that crop.
               If CropRow < Grid.RowCount AndAlso Not IsDBNull(Grid.Rows(CropRow).Cells(4).Value) Then
                  Dim CropName As String = Grid.Rows(CropRow).Cells(4).Value
                  Dim CropComponent As Component = Controller.ApsimData.Find(NodePath).FindComponentInPaddock(Controller.ApsimData.Find(NodePath), CropName)
                  If Not IsNothing(CropComponent) Then
                     CropName = CropComponent.Type
                  End If
                  Dim Classes() As String = Types.Instance.Classes(CropName)
                  Grid.CreateComboInCell(NewRow.Cells(4), Classes)
               End If

            Case "date"
               If InRefresh AndAlso Not IsDBNull(NewRow.Cells(4).Value) AndAlso NewRow.Cells(4).Value <> "" Then
                  Try
                     Dim Value As DateTime = DateTime.ParseExact(NewRow.Cells(4).Value, "d/M/yyyy", Nothing)
                     NewRow.Cells(4).Value = Value.ToShortDateString()
                  Catch ex As Exception
                     NewRow.Cells(4).Value = ""
                  End Try

               End If
               Dim DateFormat As DateTimeFormatInfo = CultureInfo.CurrentCulture.DateTimeFormat
               NewRow.Cells(4).ToolTipText = "Format: " + DateFormat.ShortDatePattern
         End Select
      End If
   End Sub
   Private Sub GotoEditMode(ByVal EditMode As Boolean)
      ' ------------------------------------------------------------
      ' Turn the grid into edit mode or not depending on the boolean
      ' passed in.
      ' ------------------------------------------------------------

      If Data.Name.ToLower() = "soil" Then
         Grid.ContextMenuStrip = Nothing
         Grid.PopupMenu = Nothing
      Else
         For i As Integer = 2 To Grid.PopupMenu.Items.Count - 1
            Grid.PopupMenu.Items(i).Enabled = EditMode
         Next
      End If
      EditModeItem.Checked = EditMode

      If Grid.Columns.Count > 0 Then
         Grid.Columns(0).Visible = EditMode        ' name 
         Grid.Columns(1).Visible = EditMode        ' type
         Grid.Columns(2).Visible = EditMode        ' list items
         Grid.Columns(3).ReadOnly = Not EditMode   ' description (only in edit mode is it editable)
      End If
      Grid.AllowUserToAddRows = EditMode
   End Sub
   Public ReadOnly Property IsEmpty() As Boolean
      ' ------------------------------------------------------------
      ' Called by ProfileUI to determine if we have anything to
      ' display.
      ' ------------------------------------------------------------

      Get
         Return Grid.RowCount = 0
      End Get
   End Property
   Private Sub OnEditModeClick(ByVal sender As System.Object, ByVal e As EventArgs)
      ' ------------------------------------------------------------
      ' Toggle edit mode
      ' ------------------------------------------------------------
      GotoEditMode(Not EditModeItem.Checked)
   End Sub
   Private Sub OnCellContentClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles Grid.CellContentClick
      If TypeOf Grid.CurrentCell Is DataGridViewButtonCell Then
         ' --------------------------------------------------------------
         ' User has clicked a button in a cell somewhere on our grid.
         ' Pass event to BaseController so that it can act on it.
         ' --------------------------------------------------------------
         Dim Button As DataGridViewButtonCell = Grid.CurrentCell
         Dim Dialog As New OpenFileDialog
         Dialog.AddExtension = True
         Dialog.RestoreDirectory = True
         If Dialog.ShowDialog = DialogResult.OK Then
            Dim Text As String = ""
            For Each FileName As String In Dialog.FileNames
               Text += FileName + vbCrLf
            Next
            Grid.Rows(e.RowIndex).Cells(e.ColumnIndex).Value = Text
         End If
      End If
   End Sub
   Private Sub OnTableColumnChanged(ByVal ColumnNames As List(Of String)) Handles Grid.TableColumnChangedEvent
      ' --------------------------------------------------------------
      ' Cell value has changed. Give value back to our editor class.
      ' --------------------------------------------------------------
      'If ColumnNames.Contains("Type") Or TypeOf Grid.CurrentCell Is DataGridViewComboBoxCell Then
      ' User has changed the type. Need to recreate the row.
      Dim CurrentCol As Integer = Grid.CurrentCell.ColumnIndex
      Dim CurrentRow As Integer = Grid.CurrentCell.RowIndex

      Grid.PopulateGrid()
      'End If

      ' Size the grid columns sensibly
      Grid.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.None
      For Col As Integer = 0 To Grid.Columns.Count - 1
         Grid.Columns(Col).Width = Grid.Columns(Col).GetPreferredWidth(DataGridViewAutoSizeColumnMode.AllCells, True)
         Grid.Columns(Col).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft
      Next
      Grid.Columns(4).AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill

      Grid.CurrentCell = Grid.Rows(CurrentRow).Cells(CurrentCol)
      IsDirty = True
   End Sub


   Private Sub OnMemoTextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBox.TextChanged
      IsDirty = True
   End Sub
End Class

