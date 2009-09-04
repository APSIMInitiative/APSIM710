
Imports System.Collections
Imports System.Collections.Generic
Imports System.Collections.Specialized
Imports System.IO
Imports System.Xml

Imports FarPoint.Win.Spread

Imports ApsimFile
Imports Controllers
Imports CSGeneral
Imports UIUtility   'GridUtility.cs , CheckedListBoxCellType.cs
Imports System.Globalization


Public Class GenericUI
    Inherits BaseView

    Private Empty As FarPoint.Win.Spread.CellType.EmptyCellType = New FarPoint.Win.Spread.CellType.EmptyCellType

#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        'Add any initialization after the InitializeComponent() call
        Dim InputMap As FarPoint.Win.Spread.InputMap = FpSpread1.GetInputMap(FarPoint.Win.Spread.InputMapMode.WhenAncestorOfFocused)
        InputMap.Put(New FarPoint.Win.Spread.Keystroke(Keys.Delete, Keys.None), _
                    FarPoint.Win.Spread.SpreadActions.ClipboardCut)
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
    Friend WithEvents FpSpread1 As FarPoint.Win.Spread.FpSpread
    Friend WithEvents PopupMenu As System.Windows.Forms.ContextMenuStrip
    Friend WithEvents EditModeItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents Grid As FarPoint.Win.Spread.SheetView
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim TipAppearance1 As FarPoint.Win.Spread.TipAppearance = New FarPoint.Win.Spread.TipAppearance
        Dim ComboBoxCellType1 As FarPoint.Win.Spread.CellType.ComboBoxCellType = New FarPoint.Win.Spread.CellType.ComboBoxCellType
        Me.PictureBox = New System.Windows.Forms.PictureBox
        Me.FpSpread1 = New FarPoint.Win.Spread.FpSpread
        Me.PopupMenu = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.EditModeItem = New System.Windows.Forms.ToolStripMenuItem
        Me.Grid = New FarPoint.Win.Spread.SheetView
        CType(Me.PictureBox, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.FpSpread1, System.ComponentModel.ISupportInitialize).BeginInit()
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
        Me.PictureBox.Dock = System.Windows.Forms.DockStyle.Left
        Me.PictureBox.Location = New System.Drawing.Point(0, 16)
        Me.PictureBox.Name = "PictureBox"
        Me.PictureBox.Size = New System.Drawing.Size(88, 701)
        Me.PictureBox.TabIndex = 3
        Me.PictureBox.TabStop = False
        '
        'FpSpread1
        '
        Me.FpSpread1.AccessibleDescription = "FpSpread1, Sheet1, Row 0, Column 0, "
        Me.FpSpread1.BackColor = System.Drawing.SystemColors.Window
        Me.FpSpread1.ClipboardOptions = FarPoint.Win.Spread.ClipboardOptions.NoHeaders
        Me.FpSpread1.ContextMenuStrip = Me.PopupMenu
        Me.FpSpread1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.FpSpread1.EditModeReplace = True
        Me.FpSpread1.HorizontalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded
        Me.FpSpread1.Location = New System.Drawing.Point(88, 16)
        Me.FpSpread1.Name = "FpSpread1"
        Me.FpSpread1.Sheets.AddRange(New FarPoint.Win.Spread.SheetView() {Me.Grid})
        Me.FpSpread1.Size = New System.Drawing.Size(934, 701)
        Me.FpSpread1.TabIndex = 4
        Me.FpSpread1.TabStripPolicy = FarPoint.Win.Spread.TabStripPolicy.Never
        Me.FpSpread1.TabStripRatio = 0.284684684684685
        TipAppearance1.BackColor = System.Drawing.SystemColors.Info
        TipAppearance1.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        TipAppearance1.ForeColor = System.Drawing.SystemColors.InfoText
        Me.FpSpread1.TextTipAppearance = TipAppearance1
        Me.FpSpread1.TextTipPolicy = FarPoint.Win.Spread.TextTipPolicy.Floating
        Me.FpSpread1.VerticalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded
        Me.FpSpread1.SetViewportPreferredWidth(0, 532)
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
        Me.Grid.Reset()
        'Formulas and custom names must be loaded with R1C1 reference style
        Me.Grid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1
        Me.Grid.ColumnCount = 6
        Me.Grid.AutoUpdateNotes = True
        Me.Grid.ColumnHeader.Cells.Get(0, 0).Value = "Name"
        Me.Grid.ColumnHeader.Cells.Get(0, 1).Value = "Type"
        Me.Grid.ColumnHeader.Cells.Get(0, 2).Value = "List items (csv)"
        Me.Grid.ColumnHeader.Cells.Get(0, 3).Value = "Description"
        Me.Grid.ColumnHeader.Cells.Get(0, 4).Value = "Value"
        Me.Grid.ColumnHeader.Cells.Get(0, 5).Value = " "
        Me.Grid.Columns.Get(0).BackColor = System.Drawing.Color.LavenderBlush
        Me.Grid.Columns.Get(0).Label = "Name"
        Me.Grid.Columns.Get(0).Width = 75.0!
        Me.Grid.Columns.Get(1).BackColor = System.Drawing.Color.LavenderBlush
        ComboBoxCellType1.ButtonAlign = FarPoint.Win.ButtonAlign.Right
        ComboBoxCellType1.Editable = True
        ComboBoxCellType1.Items = New String() {"text", "date", "yesno", "crop", "cultivars", "classes", "modulename", "list", "multilist", "category", "filename", "multiedit"}
        ComboBoxCellType1.MaxDrop = 12
        Me.Grid.Columns.Get(1).CellType = ComboBoxCellType1
        Me.Grid.Columns.Get(1).Label = "Type"
        Me.Grid.Columns.Get(1).Width = 92.0!
        Me.Grid.Columns.Get(2).BackColor = System.Drawing.Color.LavenderBlush
        Me.Grid.Columns.Get(2).Label = "List items (csv)"
        Me.Grid.Columns.Get(2).Width = 91.0!
        Me.Grid.Columns.Get(3).Label = "Description"
        Me.Grid.Columns.Get(3).Locked = True
        Me.Grid.Columns.Get(3).Width = 272.0!
        Me.Grid.Columns.Get(4).Label = "Value"
        Me.Grid.Columns.Get(4).Width = 206.0!
        Me.Grid.Columns.Get(5).Label = " "
        Me.Grid.Columns.Get(5).Visible = False
        Me.Grid.Columns.Get(5).Width = 21.0!
        Me.Grid.RowHeader.Columns.Default.Resizable = False
        Me.Grid.RowHeader.Visible = False
        Me.Grid.SheetName = "Sheet1"
        Me.Grid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1
        '
        'GenericUI
        '
        Me.Controls.Add(Me.FpSpread1)
        Me.Controls.Add(Me.PictureBox)
        Me.Name = "GenericUI"
        Me.Size = New System.Drawing.Size(1022, 717)
        Me.Controls.SetChildIndex(Me.MyHelpLabel, 0)
        Me.Controls.SetChildIndex(Me.PictureBox, 0)
        Me.Controls.SetChildIndex(Me.FpSpread1, 0)
        CType(Me.PictureBox, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.FpSpread1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.PopupMenu.ResumeLayout(False)
        CType(Me.Grid, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub

#End Region

    Protected Overrides Sub OnLoad()
        MyBase.OnLoad()
        Dim InputMap As FarPoint.Win.Spread.InputMap = FpSpread1.GetInputMap(FarPoint.Win.Spread.InputMapMode.WhenAncestorOfFocused)

        InputMap.Put(New FarPoint.Win.Spread.Keystroke(Keys.Delete, Keys.None), _
                    FarPoint.Win.Spread.SpreadActions.ClipboardCut)
        InputMap.Put(New FarPoint.Win.Spread.Keystroke(Keys.Enter, Keys.None), _
                    FarPoint.Win.Spread.SpreadActions.MoveToNextRow)
    End Sub

    Public Overrides Sub OnRefresh()
        ' --------------------------------------------------------------------
        ' Refresh this user interface
        ' --------------------------------------------------------------------
        'InRefresh = True

        Dim Comp As ApsimFile.Component = Controller.ApsimData.Find(NodePath)

        ' set the banner image correctly.
        Dim imagefile As String = Types.Instance.MetaData(Comp.Type, "image")
        If imagefile <> "" And File.Exists(imagefile) Then
            PictureBox.BackgroundImage = Drawing.Image.FromFile(imagefile)
        End If

        Grid.Columns(0).Visible = False
        Grid.Columns(1).Visible = False
        Grid.Columns(2).Visible = False
        EditModeItem.Checked = False

        ' Populate the grid.
        Grid.RowCount = 0
        Grid.RowCount = 1000
        Dim Row As Integer = 0
        PopulateGrid(Data, Row)
        Grid.RowCount = Math.Max(Row, 1)

        'InRefresh = False
    End Sub
    Private Sub PopulateGrid(ByVal Data As XmlNode, ByRef Row As Integer)
        ' --------------------------------------------------------------------
        ' Add a group of properties to grid for the specified data. Row is 
        ' updated and returned to caller.
        ' --------------------------------------------------------------------
        For Each Prop As XmlNode In XmlHelper.ChildNodes(Data, "")
            If Prop.Name = "category" Then
                Grid.Cells(Row, 1).Text = "category"
                Grid.Cells(Row, 3).Text = XmlHelper.Attribute(Prop, "description")
                If Grid.Cells(Row, 3).Text = "" Then                        'if the property does not have a description specified (because there was no value specified for the description in the xml that was passed in [properties between ui tags])
                    Grid.Cells(Row, 3).Text = XmlHelper.Name(Prop)              'use the name of the property as the description (in this case "description")
                End If
            ElseIf Prop.Name <> "condition" Then
                Grid.Cells(Row, 0).Text = Prop.Name
                Grid.Cells(Row, 1).Text = XmlHelper.Attribute(Prop, "type")
                If Grid.Cells(Row, 1).Text = "" Then                        'if the property does not have a type specified (because there was no value specified for the type in the xml that was passed in [properties between ui tags])
                    Grid.Cells(Row, 1).Text = "text"                            'make its type a text box
                End If
                Grid.Cells(Row, 2).Text = XmlHelper.Attribute(Prop, "listvalues")
                Grid.Cells(Row, 3).Text = XmlHelper.Attribute(Prop, "description")
                If Grid.Cells(Row, 3).Text = "" Then                        'if the property does not have a description specified (because there was no value specified for the description in the xml that was passed in [properties between ui tags])
                    Grid.Cells(Row, 3).Text = XmlHelper.Name(Prop)              'use the name of the property as the description (in this case "description")
                End If
                If Grid.Cells(Row, 1).Text = "date" Then
                    Dim myDTFI As DateTimeFormatInfo = CultureInfo.CurrentCulture.DateTimeFormat

                    Dim D As DateTime = DateTime.ParseExact(Prop.InnerText, "d/MM/yyyy", Nothing)
                    Grid.Cells(Row, 3).Text = Grid.Cells(Row, 3).Text + " (" + myDTFI.ShortDatePattern + ")"
                    Grid.Cells(Row, 4).Text = D.ToShortDateString()
                Else
                    Grid.Cells(Row, 4).Text = Prop.InnerText                        'assign the value column to the inner text of the property. 
                End If

            End If
            Row = Row + 1
        Next
    End Sub

    Private InCellChanged As Boolean = False
    Private Sub Grid_CellChanged(ByVal sender As Object, ByVal e As FarPoint.Win.Spread.SheetViewEventArgs) Handles Grid.CellChanged
        ' --------------------------------------------------------------------
        ' User has changed something - see if we need to create editors or
        ' setup other columns.
        ' --------------------------------------------------------------------

        If Not InCellChanged Then

            InCellChanged = True

            'Changed a cell in the "Type" column

            If e.Column = 1 Then

                'change the cell type of the "Value" column to what the user has selected. (types are text boxes, combo boxes(drop down list), date, etc.)
                CreateCellEditorForRow(e.Row)

            End If


            'Changed a cell in the "List Items" column 

            If (e.Column = 2) Then

                'Only list or multilist as its type use the "List Items" column to fill the combo box (drop down list)

                If Grid.Cells(e.Row, 1).Text = "list" Then
                    Dim Combo As FarPoint.Win.Spread.CellType.ComboBoxCellType = Grid.Cells(e.Row, 4).CellType  'assign a pointer to the "Value" combo box 
                    If Not IsNothing(Combo) Then                                                                'if the assigning of the pointer worked.
                        Combo.Items = Grid.Cells(e.Row, 2).Text.Split(",")                                          'replace the items of the "Value" combo box to the new items specified in the "List Items" column.   
                        If Grid.Cells(e.Row, 4).Text = "" And Combo.Items.Length > 0 Then                           'If in there is no specified value for this property and the combo box is NOT empty
                            Grid.Cells(e.Row, 4).Text = Combo.Items(0)                                                  'then make the first item in the combo box the selected value for the property
                        End If
                    End If
                End If

                If Grid.Cells(e.Row, 1).Text = "multilist" Then                                                 'same as above but with a "CheckedListBox Cell Type" instead of a "ComboBox Cell Type".
                    Dim CheckedList As UIUtility.CheckedListBoxCellType = Grid.Cells(e.Row, 4).CellType
                    If Not IsNothing(CheckedList) Then
                        CheckedList.Items = Grid.Cells(e.Row, 2).Text.Split(",")
                        If Grid.Cells(e.Row, 4).Text = "" And CheckedList.Items.Length > 0 Then
                            Grid.Cells(e.Row, 4).Text = CheckedList.Items(0)
                        End If
                    End If
                End If

            End If


            'Changed a cell in the "Values" column where the value that was changed was the crop.

            If (e.Column = 4) And Grid.Cells(e.Row, 1).Text.ToLower = "crop" Then

                'If the user chooses a different crop from the crop combo box, then change the NEXT cultivar property below it to show cultivars for the new crop.

                'Try and locate any rows with cultivar as the type. (start searching below the crop property until you find one, then stop)
                Dim CultRow As Integer
                For CultRow = e.Row To UIUtility.GridUtility.FindFirstBlankCell(Grid, 1) - 1
                    'If we found a cultivar row then 
                    If Grid.Cells(CultRow, 1).Text.ToLower = "cultivars" Then
                        'get rid of the value for the old crop. It won't make sense with new crop
                        Grid.Cells(CultRow, 4).Text = ""
                        'go and get all cultivars for the new crop.
                        Dim Combo As FarPoint.Win.Spread.CellType.ComboBoxCellType = Grid.Cells(CultRow, 4).CellType
                        Dim Us As ApsimFile.Component = Controller.ApsimData.Find(NodePath)
                        Dim CropComponent As Component = Controller.ApsimData.RootComponent.FindComponentInPaddock(Us, Grid.Cells(e.Row, 4).Text)
                        Combo.Items = Types.Instance.Cultivars(CropComponent.Type)
                        Exit For                    'stop looking for cultivar properties once you found the next one after the crop. 
                    ElseIf Grid.Cells(CultRow, 1).Text.ToLower = "class" Then
                        'get rid of the value for the old crop. It won't make sense with new crop
                        Grid.Cells(CultRow, 4).Text = ""
                        'go and get all cultivars for the new crop.
                        Dim Combo As FarPoint.Win.Spread.CellType.ComboBoxCellType = Grid.Cells(CultRow, 4).CellType
                        Dim Us As ApsimFile.Component = Controller.ApsimData.Find(NodePath)
                        Dim CropComponent As Component = Controller.ApsimData.RootComponent.FindComponentInPaddock(Us, Grid.Cells(e.Row, 4).Text)
                        Combo.Items = Types.Instance.Classes(CropComponent.Type)
                        Exit For                    'stop looking for cultivar properties once you found the next one after the crop. 
                    End If
                Next

            End If


            InCellChanged = False

        End If

    End Sub

    Public Overrides Sub OnSave()
        ' --------------------------------------------------------------
        ' Save all our changes back to Data
        ' --------------------------------------------------------------
        If Not IsNothing(Me.FpSpread1.ActiveSheet.ActiveCell) AndAlso Not IsNothing(Me.FpSpread1.ActiveSheet.ActiveCell.Editor) Then
            Me.FpSpread1.ActiveSheet.ActiveCell.Editor.StopEditing()
        End If
        Data.RemoveAll()
        For Row As Integer = 0 To UIUtility.GridUtility.FindFirstBlankCell(Grid, 1) - 1
            Dim DataType As String = Grid.Cells(Row, 1).Text
            If DataType <> "" Then
                Dim Type As String = Grid.Cells(Row, 0).Text
                If Type = "" Then
                    Dim Category As XmlNode = Data.AppendChild(Data.OwnerDocument.CreateElement("category"))
                    XmlHelper.SetAttribute(Category, "description", Grid.Cells(Row, 3).Text)
                Else
                    Dim NewNode As XmlNode = Data.AppendChild(Data.OwnerDocument.CreateElement(Type))
                    XmlHelper.SetAttribute(NewNode, "type", DataType)
                    If Grid.Cells(Row, 2).Text <> "" Then
                        XmlHelper.SetAttribute(NewNode, "listvalues", Grid.Cells(Row, 2).Text)
                    End If
                    Dim Desc As String = Grid.Cells(Row, 3).Text
                    If DataType = "date" Then
                        StringManip.SplitOffBracketedValue(Desc, "(", ")")
                    End If
                    XmlHelper.SetAttribute(NewNode, "description", Desc)
                    NewNode.InnerText = Grid.Cells(Row, 4).Text
                    If DataType = "date" Then
                        Dim D As DateTime = DateTime.Parse(NewNode.InnerText)
                        NewNode.InnerText = D.ToString("d/MM/yyyy")
                    End If
                End If
            End If
        Next
    End Sub

    Private Sub FpSpread1_ButtonClicked(ByVal sender As System.Object, ByVal e As FarPoint.Win.Spread.EditorNotifyEventArgs) Handles FpSpread1.ButtonClicked
        ' --------------------------------------------------------------
        ' User has clicked a button in a cell somewhere on our grid.
        ' Pass event to BaseController so that it can act on it.
        ' --------------------------------------------------------------
        Dim Dialog As New OpenFileDialog
        Dialog.AddExtension = True
        Dialog.RestoreDirectory = True
        If Dialog.ShowDialog = DialogResult.OK Then
            Dim Text As String = ""
            For Each FileName As String In Dialog.FileNames
                Text += FileName + vbCrLf
            Next
            Grid.Cells(e.Row, 1).Value = Text
        End If
    End Sub

    Public Sub CreateCellEditorForRow(ByVal Row As Integer)
        ' --------------------------------------------------------------------
        ' Change the cell type of the "Value" column to what the user has selected in the "Type" combo box(drop down list). 
        ' (types are text boxes, combo boxes(drop down list), date, etc.)
        ' --------------------------------------------------------------------

        Dim Type As String = Grid.Cells(Row, 1).Text.ToLower

        Grid.Rows(Row).BackColor = System.Drawing.Color.White
        Grid.Rows(Row).Height = 20              'reset the default row height. "multilist" & "multiedit" changes the row height this just changes it back again.  
        Grid.Cells(Row, 4).Text = ""            'get rid of the old value. Since we are changing the type of this property. The old value won't make any sense for the new type. User must enter a new value if they change the type.

        'Initialize the "List Items" column cell type.
        Dim ListItemsText As FarPoint.Win.Spread.CellType.TextCellType = New FarPoint.Win.Spread.CellType.TextCellType     'set "List Items" column cell type to text. Tried to find where in "Designer generated code" at the top,  the column cell types are initialized but couldn't. So just put it here.
        ListItemsText.MaxLength = 5000              ''default MaxLength for TextCellType is 250 characters. We need more.
        Grid.Cells(Row, 2).CellType = ListItemsText

        'Lock the "List Item" column, except if the "Type" column is a list or multilist. (both of which use the "List Items" column)
        If Type = "list" Or Type = "multilist" Then
            Grid.Cells(Row, 2).Locked = False       'unlock "List Items" column
        Else
            Grid.Cells(Row, 2).Text = ""            'clear the "List Items" column 
            Grid.Cells(Row, 2).Locked = True        'lock the "List Items" column
        End If


        'Get rid of the the old button if we have changed a filename type to some other type. 
        ' And if there is still a filename row in the grid, then make the button column visible. Otherwise make it invisible. 
        Grid.Columns(5).Visible = False
        Dim OldFileRow As Integer
        For OldFileRow = 0 To UIUtility.GridUtility.FindFirstBlankCell(Grid, 1) - 1
            ' Try and locate a row that USED TO BE a filename.
            '(check each cell in the Button column for the tag I gave it) And if you find it (check to see if the row is NOT still a filename)
            If (Grid.Cells(OldFileRow, 5).Tag = "buttoncell") AndAlso (Grid.Cells(OldFileRow, 1).Text.ToLower <> "filename") Then
                Grid.Cells(OldFileRow, 5).CellType = Empty              'remove the button. (so won't show up again, if we add a filename to another row)
            End If
            'If there is a filename row somewhere in the grid.
            If (Grid.Cells(OldFileRow, 1).Text.ToLower = "filename") Then
                Grid.Columns(5).Visible = True                         'make the button column visible
            End If
        Next




        Select Case Type

            Case "yesno"
                Dim Combo As FarPoint.Win.Spread.CellType.ComboBoxCellType = New FarPoint.Win.Spread.CellType.ComboBoxCellType
                Combo.Editable = False
                Combo.Items = New String() {"yes", "no"}
                Grid.Cells(Row, 4).CellType = Combo

            Case "list"
                Dim Combo As FarPoint.Win.Spread.CellType.ComboBoxCellType = New FarPoint.Win.Spread.CellType.ComboBoxCellType
                Combo.Editable = True                           'allow the user to directly add stuff to the combo box.
                Combo.Items = Grid.Cells(Row, 2).Text.Split(",")
                Grid.Cells(Row, 4).CellType = Combo

            Case "multilist"
                Dim CheckedList As UIUtility.CheckedListBoxCellType = New CheckedListBoxCellType
                CheckedList.Items = Grid.Cells(Row, 2).Text.Split(",")
                Grid.Cells(Row, 4).CellType = CheckedList
                Grid.Rows(Row).Height = 80

            Case "multiedit"
                Dim Text As FarPoint.Win.Spread.CellType.TextCellType = New FarPoint.Win.Spread.CellType.TextCellType
                Text.Multiline = True
                Text.MaxLength = 5000           'default MaxLength for TextCellType is 250 characters
                Grid.Cells(Row, 4).CellType = Text
                Grid.Rows(Row).Height = 80

            Case "filename"
                Grid.Cells(Row, 4).CellType = Nothing                       'This actually makes the cell a text cell type. This must be the default for the Grid. 
                Dim Button As FarPoint.Win.Spread.CellType.ButtonCellType = New FarPoint.Win.Spread.CellType.ButtonCellType
                Button.Picture = My.Resources.folder
                Grid.Columns(5).Visible = True                              'make button column visible.
                Grid.Cells(Row, 5).CellType = Button
                Grid.Cells(Row, 5).Tag = "buttoncell"                       'need to set this, so that if the type is changed to something else, I can find this cell again.

            Case "category"
                Empty.ReadOnly = True
                Grid.Rows(Row).BackColor = System.Drawing.Color.LightSteelBlue
                Grid.Cells(Row, 4).CellType = Empty                         '"Value" cell empty.

            Case "modulename"
                Dim Combo As FarPoint.Win.Spread.CellType.ComboBoxCellType = New FarPoint.Win.Spread.CellType.ComboBoxCellType
                Combo.Editable = True                                       'allow the user to directly add stuff to the combo box.
                Grid.Cells(Row, 4).CellType = Combo
                Dim Paddock As ApsimFile.Component = Controller.ApsimData.Find(NodePath).FindContainingPaddock()
                While Not IsNothing(Paddock) AndAlso Paddock.Type.ToLower = "folder"
                    Paddock = Paddock.Parent
                End While

                If Not IsNothing(Paddock) Then
                    Combo.Items = Paddock.ChildNames
                End If

            Case "crop"
                Dim Combo As FarPoint.Win.Spread.CellType.ComboBoxCellType = New FarPoint.Win.Spread.CellType.ComboBoxCellType
                Combo.Editable = True                                       'allow the user to directly add stuff to the combo box.
                Grid.Cells(Row, 4).CellType = Combo
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
                    Combo.Items = CropNames
                End If

            Case "cultivars"
                Dim Combo As FarPoint.Win.Spread.CellType.ComboBoxCellType = New FarPoint.Win.Spread.CellType.ComboBoxCellType
                Combo.Editable = True                                       'allow the user to directly add stuff to the combo box.
                Grid.Cells(Row, 4).CellType = Combo
                ' Try and locate a row with crop as the name.
                Dim CropRow As Integer
                For CropRow = 0 To UIUtility.GridUtility.FindFirstBlankCell(Grid, 1) - 1
                    If Grid.Cells(CropRow, 0).Text.ToLower = "crop" Then
                        Exit For
                    End If
                Next
                ' If we found a crop row then go and get all cultivars for that crop.
                If CropRow < UIUtility.GridUtility.FindFirstBlankCell(Grid, 1) - 1 Then
                    Combo.Items = Types.Instance.Cultivars(Grid.Cells(CropRow, 4).Text)
                End If

            Case "class"
                Dim Combo As FarPoint.Win.Spread.CellType.ComboBoxCellType = New FarPoint.Win.Spread.CellType.ComboBoxCellType
                Combo.Editable = True                                       'allow the user to directly add stuff to the combo box.
                Grid.Cells(Row, 4).CellType = Combo
                ' Try and locate a row with crop as the name.
                Dim CropRow As Integer
                For CropRow = 0 To UIUtility.GridUtility.FindFirstBlankCell(Grid, 1) - 1
                    If Grid.Cells(CropRow, 0).Text.ToLower = "crop" Then
                        Exit For
                    End If
                Next
                ' If we found a crop row then go and get all cultivars for that crop.
                If CropRow < UIUtility.GridUtility.FindFirstBlankCell(Grid, 1) - 1 Then
                    Combo.Items = Types.Instance.Classes(Grid.Cells(CropRow, 4).Text)
                End If


            Case Else       'if type is none of the above. ("ddmmmdate", "text", etc.)
                Grid.Cells(Row, 4).CellType = Nothing   'This actually makes the cell a text cell type. This must be the default for the Grid. 

        End Select


    End Sub

    Private Sub OnEditModeClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.ToolStripItemClickedEventArgs) Handles PopupMenu.ItemClicked
        EditModeItem.Checked = Not EditModeItem.Checked
        Grid.Columns(0).Visible = EditModeItem.Checked      'name 
        Grid.Columns(1).Visible = EditModeItem.Checked      'type
        Grid.Columns(2).Visible = EditModeItem.Checked      'list items
        Grid.Columns(3).Locked = Not EditModeItem.Checked   'description (only in edit mode is it editable)
        If EditModeItem.Checked Then    'if in edit mode, 
            Grid.RowCount = 500         '   force the grid to have 500 rows (plenty of blank rows for adding new properties to the managment rule)
        Else
            Grid.RowCount = UIUtility.GridUtility.FindFirstBlankCell(Grid, 1)   'else grid has just the number or rows it needs to display all the properties.
        End If
    End Sub
End Class
