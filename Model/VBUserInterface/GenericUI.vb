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
    Implements GenericUIView

    'Private Empty As FarPoint.Win.Spread.CellType.EmptyCellType = New FarPoint.Win.Spread.CellType.EmptyCellType
    Private Editor As GenericUIEditor
    Private IsDirty As Boolean

#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

    End Sub

    'Form overrides dispose to clean up the component list.
    Protected Overloads Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing Then
            If Not (components Is Nothing) Then
                components.Dispose()
            End If
        End If
        MyBase.Dispose(disposing)
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    Friend WithEvents PictureBox As System.Windows.Forms.PictureBox
    Friend WithEvents PopupMenu As System.Windows.Forms.ContextMenuStrip
    Friend WithEvents Grid As System.Windows.Forms.DataGridView
    Friend WithEvents EditModeItem As System.Windows.Forms.ToolStripMenuItem
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Me.PictureBox = New System.Windows.Forms.PictureBox
        Me.PopupMenu = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.EditModeItem = New System.Windows.Forms.ToolStripMenuItem
        Me.Grid = New System.Windows.Forms.DataGridView
        CType(Me.PictureBox, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.PopupMenu.SuspendLayout()
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
        'PopupMenu
        '
        Me.PopupMenu.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.EditModeItem})
        Me.PopupMenu.Name = "PopupMenu"
        Me.PopupMenu.Size = New System.Drawing.Size(133, 26)
        '
        'EditModeItem
        '
        Me.EditModeItem.Name = "EditModeItem"
        Me.EditModeItem.Size = New System.Drawing.Size(132, 22)
        Me.EditModeItem.Text = "Edit mode"
        '
        'Grid
        '
        Me.Grid.AllowUserToResizeRows = False
        Me.Grid.BackgroundColor = System.Drawing.SystemColors.Window
        Me.Grid.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.Grid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.Grid.ContextMenuStrip = Me.PopupMenu
        Me.Grid.Dock = System.Windows.Forms.DockStyle.Fill
        Me.Grid.Location = New System.Drawing.Point(125, 16)
        Me.Grid.Name = "Grid"
        Me.Grid.RowHeadersVisible = False
        Me.Grid.Size = New System.Drawing.Size(897, 701)
        Me.Grid.TabIndex = 4
        '
        'GenericUI
        '
        Me.Controls.Add(Me.Grid)
        Me.Controls.Add(Me.PictureBox)
        Me.Name = "GenericUI"
        Me.Size = New System.Drawing.Size(1022, 717)
        Me.Controls.SetChildIndex(Me.MyHelpLabel, 0)
        Me.Controls.SetChildIndex(Me.PictureBox, 0)
        Me.Controls.SetChildIndex(Me.Grid, 0)
        CType(Me.PictureBox, System.ComponentModel.ISupportInitialize).EndInit()
        Me.PopupMenu.ResumeLayout(False)
        CType(Me.Grid, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

#End Region

    Protected Overrides Sub OnLoad()
        MyBase.OnLoad()
        Editor = New GenericUIEditor(Data, Me)

    End Sub

    Public Overrides Sub OnRefresh()
        ' --------------------------------------------------------------------
        ' Refresh this user interface
        ' --------------------------------------------------------------------
        'InRefresh = True

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

        ' populate the grid.
        Editor.RefreshView()

        GotoEditMode(False)
    End Sub

    Public Sub SetTable(ByVal Table As System.Data.DataTable) Implements GenericUIView.SetTable
        ' --------------------------------------------------------------
        ' Our editor is giving us a datatable to populate ourselves.
        ' --------------------------------------------------------------

        Grid.Rows.Clear()
        Grid.ColumnCount = Table.Columns.Count
        Grid.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.None
        For Col As Integer = 0 To Table.Columns.Count - 1
            Grid.Columns(Col).HeaderText = Table.Columns(Col).ColumnName
            Grid.Columns(Col).SortMode = DataGridViewColumnSortMode.NotSortable
        Next
        For Each Row As DataRow In Table.Rows
            ' Only show rows that aren't XY rows. These XY rows are used in the Plant2IDE.
            If Row(0) <> "XY" Then
                Grid.Rows.Add(CreateGridRow(ToStr(Row(0)), ToStr(Row(1)), ToStr(Row(2)), ToStr(Row(3)), ToStr(Row(4))))
            End If
        Next

        ' Size the columns sensibly
        For Col As Integer = 0 To Table.Columns.Count - 1
            Grid.Columns(Col).Width = Grid.Columns(Col).GetPreferredWidth(DataGridViewAutoSizeColumnMode.AllCells, True)
        Next
        Grid.Columns(4).AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill
        IsDirty = False
    End Sub

    Private Function ToStr(ByVal value As Object) As String
        If IsDBNull(value) Then
            Return ""
        Else
            Return value.ToString
        End If
    End Function

    Public Overrides Sub OnSave()
        ' --------------------------------------------------------------
        ' Save all our changes back to Data
        ' --------------------------------------------------------------
        If Grid.IsCurrentCellInEditMode Then
            Grid.EndEdit()
        End If
        If IsDirty Then

            Dim NumValues As Integer = Grid.RowCount

            Dim Table As DataTable = New DataTable("Data")
            Table.Columns.Add(Grid.Columns(0).HeaderText, GetType(String))
            Table.Columns.Add(Grid.Columns(1).HeaderText, GetType(String))
            Table.Columns.Add(Grid.Columns(2).HeaderText, GetType(String))
            Table.Columns.Add(Grid.Columns(3).HeaderText, GetType(String))
            Table.Columns.Add(Grid.Columns(4).HeaderText, GetType(String))

            For Row As Integer = 0 To NumValues - 1
                Dim NewRow As DataRow = Table.NewRow()
                Table.Rows.Add(NewRow)
                For Col As Integer = 0 To Grid.Columns.Count - 1
                    Dim Value As String = Grid.Rows(Row).Cells(Col).Value

                    If Not IsNothing(Value) Then
                        ' Make sure we save date rows as dd/mm/yyyy.
                        If Col = 4 AndAlso Grid.Rows(Row).Cells(1).Value = "date" Then
                            Dim DateValue As DateTime = Value
                            NewRow(Col) = DateValue.ToString("dd/MM/yyyy")
                        Else
                            NewRow(Col) = Value
                        End If
                    End If
                Next
            Next
            Editor.OnDataChanged(Table)
        End If
    End Sub



    Private Function CreateGridRow(ByVal Name As Object, _
                                   ByVal Type As Object, _
                                   ByVal ListItems As Object, _
                                   ByVal Description As Object, _
                                   ByVal Value As Object) As DataGridViewRow
        ' --------------------------------------------------------------------
        ' Change the cell type of the "Value" column to what the user has 
        ' selected in the "Type" combo box(drop down list). 
        ' (types are text boxes, combo boxes(drop down list), date, etc.)
        ' --------------------------------------------------------------------
        Dim Row As DataGridViewRow = New DataGridViewRow()

        Dim TypeComboBox As New DataGridViewComboBoxCell
        TypeComboBox.Items.Add("text")
        TypeComboBox.Items.Add("date")
        TypeComboBox.Items.Add("yesno")
        TypeComboBox.Items.Add("crop")
        TypeComboBox.Items.Add("cultivars")
        TypeComboBox.Items.Add("classes")
        TypeComboBox.Items.Add("modulename")
        TypeComboBox.Items.Add("list")
        TypeComboBox.Items.Add("multilist")
        TypeComboBox.Items.Add("category")
        TypeComboBox.Items.Add("filename")
        TypeComboBox.Items.Add("multiedit")
        TypeComboBox.MaxDropDownItems = 12

        ' Create a list items edit box that can handle lots of chars.
        Dim ListItemTextBox As New DataGridViewTextBoxCell()
        ListItemTextBox.MaxInputLength = 5000

        Row.Cells.Add(New DataGridViewTextBoxCell())   ' Name
        Row.Cells.Add(TypeComboBox)                    ' Type
        Row.Cells.Add(ListItemTextBox)                 ' List items
        Row.Cells.Add(New DataGridViewTextBoxCell())   ' Description

        ' Now put in all values into the cells.
        Row.Cells(0).Value = Name
        Row.Cells(1).Value = Type
        Row.Cells(2).Value = ListItems
        Row.Cells(3).Value = Description

        AddValueCellToRow(Type, Row, Value)

        If Type = "date" Then
            If Value <> "" Then
                Row.Cells(4).Value = DateTime.ParseExact(Value, "d/M/yyyy", Nothing)
            End If
            Dim DateFormat As DateTimeFormatInfo = CultureInfo.CurrentCulture.DateTimeFormat
            Row.Cells(4).ToolTipText = "Format: " + DateFormat.ShortDatePattern
        ElseIf Not IsDBNull(Value) Then
            Row.Cells(4).Value = Value
        End If

        Return Row
    End Function
    Private Sub AddValueCellToRow(ByVal Type As String, ByVal Row As DataGridViewRow, ByVal Value As String)

        Select Case Type

            Case "yesno"
                Dim Combo As New DataGridViewComboBoxCell
                Combo.Items.Add("yes")
                Combo.Items.Add("no")
                Row.Cells.Add(Combo)

            Case "list"
                Dim Combo As New DataGridViewComboBoxCell
                Combo.Items.Add(Value)
                If Not IsNothing(Row.Cells(2).Value) Then
                    For Each St As String In Row.Cells(2).Value.Split(",")
                        Combo.Items.Add(St)
                    Next
                End If
                Row.Cells.Add(Combo)

            Case "multilist"
                Dim Combo As New DataGridViewComboBoxCell
                Combo.Items.Add(Value)
                If Not IsNothing(Row.Cells(2).Value) Then
                    For Each St As String In Row.Cells(2).Value.Split(",")
                        Combo.Items.Add(St)
                    Next
                End If
                Row.Cells.Add(Combo)

            Case "multiedit"
                Dim Text As New DataGridViewTextBoxCell()
                Text.MaxInputLength = 5000
                Text.Style.WrapMode = DataGridViewTriState.True
                Row.Height = 80
                Row.Cells.Add(Text)

            Case "filename"
                Dim Button As New DataGridViewButtonCell()
                Row.Cells.Add(Button)

            Case "category"
                Row.Cells.Add(New DataGridViewTextBoxCell())
                Row.Cells(3).ReadOnly = True  ' Make description field readonly
                Row.Cells(4).ReadOnly = True  ' Make description field readonly
                Row.DefaultCellStyle.BackColor = System.Drawing.Color.LightSteelBlue

            Case "modulename"
                Dim Combo As New DataGridViewComboBoxCell
                Row.Cells.Add(Combo)
                Dim Paddock As ApsimFile.Component = Controller.ApsimData.Find(NodePath).FindContainingPaddock()
                While Not IsNothing(Paddock) AndAlso Paddock.Type.ToLower = "folder"
                    Paddock = Paddock.Parent
                End While

                If Not IsNothing(Paddock) Then
                    For Each ModuleName As String In Paddock.ChildNames
                        Combo.Items.Add(ModuleName)
                    Next
                End If

            Case "crop"
                Dim Combo As New DataGridViewComboBoxCell
                Row.Cells.Add(Combo)

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
                    Combo.Items.Add(Value)
                    For Each Crop As String In Crops
                        Combo.Items.Add(Crop)
                    Next
                End If

            Case "cultivars"
                Dim Combo As New DataGridViewComboBoxCell
                Row.Cells.Add(Combo)
                Combo.Items.Add(Value)

                ' Try and locate a row with crop as the name.
                Dim CropRow As Integer
                For CropRow = 0 To Grid.RowCount - 1
                    If Grid.Rows(CropRow).Cells(0).Value.ToLower = "crop" Then
                        Exit For
                    End If
                Next
                ' If we found a crop row then go and get all cultivars for that crop.
                If CropRow < Grid.RowCount Then
                    Dim Cultivars() As String = Types.Instance.Cultivars(Grid.Rows(CropRow).Cells(4).Value)
                    For Each Cultivar As String In Cultivars
                        Combo.Items.Add(Cultivar)
                    Next
                End If

            Case "classes"
                Dim Combo As New DataGridViewComboBoxCell
                Row.Cells.Add(Combo)
                Combo.Items.Add(Value)

                ' Try and locate a row with crop as the name.
                Dim CropRow As Integer
                For CropRow = 0 To Grid.RowCount - 1
                    If Grid.Rows(CropRow).Cells(0).Value.ToLower = "crop" Then
                        Exit For
                    End If
                Next
                ' If we found a crop row then go and get all cultivars for that crop.
                If CropRow < Grid.RowCount Then
                    Dim Classes() As String = Types.Instance.Classes(Grid.Rows(CropRow).Cells(4).Value)
                    For Each Clss As String In Classes
                        Combo.Items.Add(Clss)
                    Next
                End If

            Case "date"
                Dim DateEditor As New CalendarCell()
                Row.Cells.Add(DateEditor)

            Case Else       'if type is none of the above. ("ddmmmdate", "text", etc.)
                Row.Cells.Add(New DataGridViewTextBoxCell())  'This actually makes the cell a text cell type. This must be the default for the Grid. 

        End Select
    End Sub
    Private Sub GotoEditMode(ByVal EditMode As Boolean)
        ' ------------------------------------------------------------
        ' Turn the grid into edit mode or not depending on the boolean
        ' passed in.
        ' ------------------------------------------------------------

        EditModeItem.Checked = EditMode

        Grid.Columns(0).Visible = EditMode        ' name 
        Grid.Columns(1).Visible = EditMode        ' type
        Grid.Columns(2).Visible = EditMode        ' list items
        Grid.Columns(3).ReadOnly = Not EditMode   ' description (only in edit mode is it editable)
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

    Private Sub OnEditingControlShowing(ByVal sender As Object, ByVal e As DataGridViewEditingControlShowingEventArgs) Handles Grid.EditingControlShowing
        ' ------------------------------------------------------------
        ' By default the combo boxes don't allow manual editing. This
        ' event handler changes that.
        ' ------------------------------------------------------------
        If TypeOf e.Control Is DataGridViewComboBoxEditingControl Then
            Dim comboControl As DataGridViewComboBoxEditingControl = e.Control
            If Not IsNothing(comboControl) Then
                ' Set the DropDown style to get an editable ComboBox
                If comboControl.DropDownStyle <> ComboBoxStyle.DropDown Then
                    comboControl.DropDownStyle = ComboBoxStyle.DropDown
                End If
            End If
        End If
    End Sub
    Private Sub OnEditModeClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.ToolStripItemClickedEventArgs) Handles PopupMenu.ItemClicked
        ' ------------------------------------------------------------
        ' Toggle edit mode
        ' ------------------------------------------------------------
        GotoEditMode(Not EditModeItem.Checked)
    End Sub
    Private Sub OnDataError(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles Grid.DataError
        ' ------------------------------------------------------------
        ' Bad combo box value. Ignore error. If this handler doesn't
        ' exist, then a messagebox goes to the user.
        ' ------------------------------------------------------------

    End Sub
    Private Sub OnCellValidating(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellValidatingEventArgs) Handles Grid.CellValidating
        ' ------------------------------------------------------------
        ' User has finished editing a cell value. If the cell is a
        ' combo cell and the new value isn't in the combo list of 
        ' allowable values then add it to the list.
        ' ------------------------------------------------------------

        If TypeOf Grid.CurrentCell Is DataGridViewComboBoxCell Then
            Dim Combo As DataGridViewComboBoxCell = Grid.CurrentCell
            If Not Combo.Items.Contains(e.FormattedValue) Then
                Combo.Items.Insert(0, e.FormattedValue)
            End If
        End If
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
    Private Sub OnCellChanged(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles Grid.CellValueChanged
        ' --------------------------------------------------------------
        ' Cell value has changed. Give value back to our editor class.
        ' --------------------------------------------------------------
        If (e.ColumnIndex = 1 Or e.ColumnIndex = 2) And e.RowIndex >= 0 Then
            ' User has changed the type. Need to recreate the row.
            Dim NewRow As DataGridViewRow = CreateGridRow(Grid.Rows(e.RowIndex).Cells(0).Value, _
                                                          Grid.Rows(e.RowIndex).Cells(1).Value, _
                                                          Grid.Rows(e.RowIndex).Cells(2).Value, _
                                                          Grid.Rows(e.RowIndex).Cells(3).Value, _
                                                          Grid.Rows(e.RowIndex).Cells(4).Value)
            Grid.Rows.RemoveAt(e.RowIndex)
            Grid.Rows.Insert(e.RowIndex, NewRow)
        End If
        IsDirty = True
    End Sub
End Class
