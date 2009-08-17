
Imports System
Imports System.Collections
Imports System.Collections.Generic
Imports System.Collections.Specialized
Imports System.IO
Imports System.Xml

Imports ApsimFile
Imports Controllers
Imports CSGeneral
Imports UIUtility   'GridUtility.cs


Public Class OutputFileDescUI
    Inherits BaseView
    Private UserChange As Boolean = True
    Private ComponentNames As New StringCollection
    Friend WithEvents ConstantsBox As System.Windows.Forms.TextBox
    Friend WithEvents Splitter1 As System.Windows.Forms.Splitter
    Friend WithEvents Splitter2 As System.Windows.Forms.Splitter
    Friend WithEvents TopPanel As System.Windows.Forms.Panel
    Friend WithEvents GridLabel As System.Windows.Forms.Label
    Friend WithEvents ConstantsLabel As System.Windows.Forms.Label
    Friend WithEvents DictionaryLabel As System.Windows.Forms.Label
    Friend WithEvents HelpButton As System.Windows.Forms.Button
    Private ComponentTypes As New StringCollection

#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        'Add any initialization after the InitializeComponent() call
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
    Friend WithEvents OpenFileDialog As System.Windows.Forms.OpenFileDialog
    Friend WithEvents RightHandPanel As System.Windows.Forms.Panel
    Friend WithEvents FpSpread1 As FarPoint.Win.Spread.FpSpread
    Friend WithEvents Grid As FarPoint.Win.Spread.SheetView
    Friend WithEvents Spread As FarPoint.Win.Spread.FpSpread
    Friend WithEvents GridContextMenu As System.Windows.Forms.ContextMenuStrip
    Friend WithEvents MoveUpMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents MoveDownMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents BottomPanel As System.Windows.Forms.Panel
    Friend WithEvents VariableListView As System.Windows.Forms.ListView
    Friend WithEvents ColumnHeader1 As System.Windows.Forms.ColumnHeader
    Friend WithEvents ColumnHeader4 As System.Windows.Forms.ColumnHeader
    Friend WithEvents ColumnHeader3 As System.Windows.Forms.ColumnHeader
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents ComponentFilter As System.Windows.Forms.ComboBox

    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(OutputFileDescUI))
        Dim TipAppearance1 As FarPoint.Win.Spread.TipAppearance = New FarPoint.Win.Spread.TipAppearance
        Me.OpenFileDialog = New System.Windows.Forms.OpenFileDialog
        Me.RightHandPanel = New System.Windows.Forms.Panel
        Me.HelpButton = New System.Windows.Forms.Button
        Me.Spread = New FarPoint.Win.Spread.FpSpread
        Me.GridContextMenu = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.MoveUpMenuItem = New System.Windows.Forms.ToolStripMenuItem
        Me.MoveDownMenuItem = New System.Windows.Forms.ToolStripMenuItem
        Me.Grid = New FarPoint.Win.Spread.SheetView
        Me.GridLabel = New System.Windows.Forms.Label
        Me.Splitter1 = New System.Windows.Forms.Splitter
        Me.Splitter2 = New System.Windows.Forms.Splitter
        Me.BottomPanel = New System.Windows.Forms.Panel
        Me.DictionaryLabel = New System.Windows.Forms.Label
        Me.VariableListView = New System.Windows.Forms.ListView
        Me.ColumnHeader1 = New System.Windows.Forms.ColumnHeader
        Me.ColumnHeader4 = New System.Windows.Forms.ColumnHeader
        Me.ColumnHeader3 = New System.Windows.Forms.ColumnHeader
        Me.Label1 = New System.Windows.Forms.Label
        Me.ComponentFilter = New System.Windows.Forms.ComboBox
        Me.TopPanel = New System.Windows.Forms.Panel
        Me.ConstantsBox = New System.Windows.Forms.TextBox
        Me.ConstantsLabel = New System.Windows.Forms.Label
        Me.RightHandPanel.SuspendLayout()
        CType(Me.Spread, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GridContextMenu.SuspendLayout()
        CType(Me.Grid, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.BottomPanel.SuspendLayout()
        Me.TopPanel.SuspendLayout()
        Me.SuspendLayout()
        '
        'MyHelpLabel
        '
        Me.MyHelpLabel.Size = New System.Drawing.Size(753, 16)
        '
        'OpenFileDialog
        '
        Me.OpenFileDialog.CheckFileExists = False
        Me.OpenFileDialog.DefaultExt = "out"
        Me.OpenFileDialog.Filter = "APSIM output files(*.out)|*.out|All Files (*.*)|*.*"
        Me.OpenFileDialog.RestoreDirectory = True
        Me.OpenFileDialog.Title = "Enter output file name"
        '
        'RightHandPanel
        '
        Me.RightHandPanel.Controls.Add(Me.HelpButton)
        Me.RightHandPanel.Controls.Add(Me.Spread)
        Me.RightHandPanel.Controls.Add(Me.GridLabel)
        Me.RightHandPanel.Controls.Add(Me.Splitter1)
        Me.RightHandPanel.Controls.Add(Me.Splitter2)
        Me.RightHandPanel.Controls.Add(Me.BottomPanel)
        Me.RightHandPanel.Controls.Add(Me.TopPanel)
        Me.RightHandPanel.Dock = System.Windows.Forms.DockStyle.Fill
        Me.RightHandPanel.Location = New System.Drawing.Point(0, 16)
        Me.RightHandPanel.Name = "RightHandPanel"
        Me.RightHandPanel.Size = New System.Drawing.Size(753, 508)
        Me.RightHandPanel.TabIndex = 11
        '
        'HelpButton
        '
        Me.HelpButton.AutoSize = True
        Me.HelpButton.Image = CType(resources.GetObject("HelpButton.Image"), System.Drawing.Image)
        Me.HelpButton.Location = New System.Drawing.Point(613, 96)
        Me.HelpButton.Name = "HelpButton"
        Me.HelpButton.Size = New System.Drawing.Size(30, 30)
        Me.HelpButton.TabIndex = 26
        Me.HelpButton.UseVisualStyleBackColor = True
        '
        'Spread
        '
        Me.Spread.AccessibleDescription = "Spread"
        Me.Spread.AllowDrop = True
        Me.Spread.ContextMenuStrip = Me.GridContextMenu
        Me.Spread.Dock = System.Windows.Forms.DockStyle.Fill
        Me.Spread.EditModeReplace = True
        Me.Spread.HorizontalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded
        Me.Spread.Location = New System.Drawing.Point(0, 93)
        Me.Spread.Name = "Spread"
        Me.Spread.Sheets.AddRange(New FarPoint.Win.Spread.SheetView() {Me.Grid})
        Me.Spread.Size = New System.Drawing.Size(753, 204)
        Me.Spread.TabIndex = 14
        TipAppearance1.BackColor = System.Drawing.SystemColors.Info
        TipAppearance1.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        TipAppearance1.ForeColor = System.Drawing.SystemColors.InfoText
        Me.Spread.TextTipAppearance = TipAppearance1
        Me.Spread.VerticalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded
        '
        'GridContextMenu
        '
        Me.GridContextMenu.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.MoveUpMenuItem, Me.MoveDownMenuItem})
        Me.GridContextMenu.Name = "ContextMenu"
        Me.GridContextMenu.Size = New System.Drawing.Size(246, 48)
        '
        'MoveUpMenuItem
        '
        Me.MoveUpMenuItem.Name = "MoveUpMenuItem"
        Me.MoveUpMenuItem.ShortcutKeys = CType((System.Windows.Forms.Keys.Control Or System.Windows.Forms.Keys.Up), System.Windows.Forms.Keys)
        Me.MoveUpMenuItem.Size = New System.Drawing.Size(245, 22)
        Me.MoveUpMenuItem.Text = "Move variables &up"
        '
        'MoveDownMenuItem
        '
        Me.MoveDownMenuItem.Name = "MoveDownMenuItem"
        Me.MoveDownMenuItem.ShortcutKeys = CType((System.Windows.Forms.Keys.Control Or System.Windows.Forms.Keys.Down), System.Windows.Forms.Keys)
        Me.MoveDownMenuItem.Size = New System.Drawing.Size(245, 22)
        Me.MoveDownMenuItem.Text = "Move variables &down"
        '
        'Grid
        '
        Me.Grid.Reset()
        'Formulas and custom names must be loaded with R1C1 reference style
        Me.Grid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1
        Me.Grid.ColumnCount = 3
        Me.Grid.AutoCalculation = False
        Me.Grid.AutoUpdateNotes = True
        Me.Grid.ColumnHeader.Cells.Get(0, 0).Value = "Variable name"
        Me.Grid.ColumnHeader.Cells.Get(0, 1).Value = "Array?"
        Me.Grid.ColumnHeader.Cells.Get(0, 2).Value = "Description"
        Me.Grid.Columns.Get(0).BackColor = System.Drawing.Color.WhiteSmoke
        Me.Grid.Columns.Get(0).Label = "Variable name"
        Me.Grid.Columns.Get(0).Width = 219.0!
        Me.Grid.Columns.Get(1).Label = "Array?"
        Me.Grid.Columns.Get(1).Locked = True
        Me.Grid.Columns.Get(1).Width = 52.0!
        Me.Grid.Columns.Get(2).Label = "Description"
        Me.Grid.Columns.Get(2).Locked = True
        Me.Grid.Columns.Get(2).Width = 338.0!
        Me.Grid.RowHeader.Columns.Default.Resizable = False
        Me.Grid.RowHeader.Visible = False
        Me.Grid.SheetName = "Sheet1"
        Me.Grid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1
        '
        'GridLabel
        '
        Me.GridLabel.BackColor = System.Drawing.SystemColors.Highlight
        Me.GridLabel.Dock = System.Windows.Forms.DockStyle.Top
        Me.GridLabel.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.GridLabel.ForeColor = System.Drawing.SystemColors.HighlightText
        Me.GridLabel.Location = New System.Drawing.Point(0, 75)
        Me.GridLabel.Name = "GridLabel"
        Me.GridLabel.Size = New System.Drawing.Size(753, 18)
        Me.GridLabel.TabIndex = 25
        Me.GridLabel.Text = "Output file columns:"
        Me.GridLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'Splitter1
        '
        Me.Splitter1.Dock = System.Windows.Forms.DockStyle.Top
        Me.Splitter1.Location = New System.Drawing.Point(0, 72)
        Me.Splitter1.Name = "Splitter1"
        Me.Splitter1.Size = New System.Drawing.Size(753, 3)
        Me.Splitter1.TabIndex = 22
        Me.Splitter1.TabStop = False
        '
        'Splitter2
        '
        Me.Splitter2.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.Splitter2.Location = New System.Drawing.Point(0, 297)
        Me.Splitter2.Name = "Splitter2"
        Me.Splitter2.Size = New System.Drawing.Size(753, 3)
        Me.Splitter2.TabIndex = 23
        Me.Splitter2.TabStop = False
        '
        'BottomPanel
        '
        Me.BottomPanel.Controls.Add(Me.DictionaryLabel)
        Me.BottomPanel.Controls.Add(Me.VariableListView)
        Me.BottomPanel.Controls.Add(Me.Label1)
        Me.BottomPanel.Controls.Add(Me.ComponentFilter)
        Me.BottomPanel.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.BottomPanel.Location = New System.Drawing.Point(0, 300)
        Me.BottomPanel.Name = "BottomPanel"
        Me.BottomPanel.Size = New System.Drawing.Size(753, 208)
        Me.BottomPanel.TabIndex = 18
        '
        'DictionaryLabel
        '
        Me.DictionaryLabel.BackColor = System.Drawing.SystemColors.Highlight
        Me.DictionaryLabel.Dock = System.Windows.Forms.DockStyle.Top
        Me.DictionaryLabel.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.DictionaryLabel.ForeColor = System.Drawing.SystemColors.HighlightText
        Me.DictionaryLabel.Location = New System.Drawing.Point(0, 0)
        Me.DictionaryLabel.Name = "DictionaryLabel"
        Me.DictionaryLabel.Size = New System.Drawing.Size(753, 18)
        Me.DictionaryLabel.TabIndex = 22
        Me.DictionaryLabel.Text = "Variables to drag onto above grid:"
        Me.DictionaryLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'VariableListView
        '
        Me.VariableListView.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
                    Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.VariableListView.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.ColumnHeader1, Me.ColumnHeader4, Me.ColumnHeader3})
        Me.VariableListView.FullRowSelect = True
        Me.VariableListView.Location = New System.Drawing.Point(11, 59)
        Me.VariableListView.Name = "VariableListView"
        Me.VariableListView.Size = New System.Drawing.Size(730, 135)
        Me.VariableListView.Sorting = System.Windows.Forms.SortOrder.Ascending
        Me.VariableListView.TabIndex = 20
        Me.VariableListView.UseCompatibleStateImageBehavior = False
        Me.VariableListView.View = System.Windows.Forms.View.Details
        '
        'ColumnHeader1
        '
        Me.ColumnHeader1.Text = "Variable name"
        Me.ColumnHeader1.Width = 201
        '
        'ColumnHeader4
        '
        Me.ColumnHeader4.Text = "Array?"
        Me.ColumnHeader4.Width = 45
        '
        'ColumnHeader3
        '
        Me.ColumnHeader3.Text = "Description"
        Me.ColumnHeader3.Width = 437
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(10, 35)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(86, 13)
        Me.Label1.TabIndex = 19
        Me.Label1.Text = "Component filter:"
        '
        'ComponentFilter
        '
        Me.ComponentFilter.FormattingEnabled = True
        Me.ComponentFilter.Location = New System.Drawing.Point(102, 32)
        Me.ComponentFilter.Name = "ComponentFilter"
        Me.ComponentFilter.Size = New System.Drawing.Size(237, 21)
        Me.ComponentFilter.TabIndex = 18
        '
        'TopPanel
        '
        Me.TopPanel.Controls.Add(Me.ConstantsBox)
        Me.TopPanel.Controls.Add(Me.ConstantsLabel)
        Me.TopPanel.Dock = System.Windows.Forms.DockStyle.Top
        Me.TopPanel.Location = New System.Drawing.Point(0, 0)
        Me.TopPanel.Name = "TopPanel"
        Me.TopPanel.Size = New System.Drawing.Size(753, 72)
        Me.TopPanel.TabIndex = 24
        '
        'ConstantsBox
        '
        Me.ConstantsBox.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.ConstantsBox.Dock = System.Windows.Forms.DockStyle.Fill
        Me.ConstantsBox.Location = New System.Drawing.Point(0, 18)
        Me.ConstantsBox.Multiline = True
        Me.ConstantsBox.Name = "ConstantsBox"
        Me.ConstantsBox.Size = New System.Drawing.Size(753, 54)
        Me.ConstantsBox.TabIndex = 19
        '
        'ConstantsLabel
        '
        Me.ConstantsLabel.BackColor = System.Drawing.SystemColors.Highlight
        Me.ConstantsLabel.Dock = System.Windows.Forms.DockStyle.Top
        Me.ConstantsLabel.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.ConstantsLabel.ForeColor = System.Drawing.SystemColors.HighlightText
        Me.ConstantsLabel.Location = New System.Drawing.Point(0, 0)
        Me.ConstantsLabel.Name = "ConstantsLabel"
        Me.ConstantsLabel.Size = New System.Drawing.Size(753, 18)
        Me.ConstantsLabel.TabIndex = 20
        Me.ConstantsLabel.Text = "Constants to put in top of output file:"
        Me.ConstantsLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'OutputFileDescUI
        '
        Me.Controls.Add(Me.RightHandPanel)
        Me.Name = "OutputFileDescUI"
        Me.Size = New System.Drawing.Size(753, 524)
        Me.Controls.SetChildIndex(Me.MyHelpLabel, 0)
        Me.Controls.SetChildIndex(Me.RightHandPanel, 0)
        Me.RightHandPanel.ResumeLayout(False)
        Me.RightHandPanel.PerformLayout()
        CType(Me.Spread, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GridContextMenu.ResumeLayout(False)
        CType(Me.Grid, System.ComponentModel.ISupportInitialize).EndInit()
        Me.BottomPanel.ResumeLayout(False)
        Me.BottomPanel.PerformLayout()
        Me.TopPanel.ResumeLayout(False)
        Me.TopPanel.PerformLayout()
        Me.ResumeLayout(False)

    End Sub

#End Region

    Overrides Sub OnRefresh()
        ' ----------------------------------
        ' Refresh the variable grid
        ' ----------------------------------


        If XmlHelper.Type(Data).ToLower = "variables" Then
            GridLabel.Text = "Output file columns:"
            DictionaryLabel.Text = "Variables to drag onto above grid:"
            Grid.Columns(0).Width = 219
            Grid.Columns(0).Label = "Variable name"
        ElseIf XmlHelper.Type(Data).ToLower = "tracker" Then
            GridLabel.Text = "Tracker variables:"
            DictionaryLabel.Text = "Example tracker variables - drag to the grid above."
            Grid.Columns(0).Width = Spread.Size.Width - 50
            Grid.Columns(0).Label = "Tracker variable"
        Else
            GridLabel.Text = "Output file frequencies:"
            DictionaryLabel.Text = "Frequency list - drag to the grid above."
            Grid.Columns(0).Width = 219
            Grid.Columns(0).Label = "Output frequency"
        End If

        ' We want to find the component that is a child of our paddock.
        Dim Paddock As ApsimFile.Component = Controller.ApsimData.Find(NodePath).FindContainingPaddock()
        GetSiblingComponents(Paddock, ComponentNames, ComponentTypes)

        UserChange = False
        PopulateComponentFilter()
        PopulateVariableGrid()
        PopulateVariableListView()
        Grid.ActiveColumnIndex = 0
        Grid.ActiveRowIndex = 0
        Spread.SetViewportTopRow(0, 0)
        UserChange = True

        If XmlHelper.Type(Data).ToLower <> "variables" Then
            VariableListView.Columns(1).Width = 0
            Grid.Columns(1).Visible = False
            Grid.Columns(2).Visible = False
            TopPanel.Visible = False
            HelpButton.Location = New Point(Grid.Columns(0).Width, 3 + Grid.Rows(0).Height)

        Else
            VariableListView.Columns(1).Width = 45
            Grid.Columns(1).Visible = True
            Grid.Columns(2).Visible = True
            TopPanel.Visible = True
            HelpButton.Location = New Point(Grid.Columns(0).Width + Grid.Columns(1).Width + Grid.Columns(2).Width, 3 + TopPanel.Height + Grid.Rows(0).Height)
            PopulateConstants()
        End If

        Dim InputMap As FarPoint.Win.Spread.InputMap = Spread.GetInputMap(FarPoint.Win.Spread.InputMapMode.WhenAncestorOfFocused)
        InputMap.Put(New FarPoint.Win.Spread.Keystroke(Keys.Enter, Keys.None), FarPoint.Win.Spread.SpreadActions.MoveToNextRow)

    End Sub

    Private Sub PopulateComponentFilter()
        ' ----------------------------------------
        ' Populate the component filter drop down
        ' ----------------------------------------
        ComponentFilter.Items.Clear()
        For Each ComponentName As String In ComponentNames
            ComponentFilter.Items.Add(ComponentName)
        Next
        If XmlHelper.Type(Data).ToLower = "tracker" Then
            ComponentFilter.Text = "tracker"
            ComponentFilter.Visible = False
        Else
            ComponentFilter.SelectedIndex = 0
        End If

    End Sub

    Private Sub PopulateVariableGrid()
        ' -----------------------------------
        ' Populate the variable grid
        ' -----------------------------------
        Grid.ClearRange(0, 0, Grid.RowCount, Grid.ColumnCount, False)
        Dim Row As Integer = 0
        For Each Variable As XmlNode In XmlHelper.ChildNodes(Data, "")
            If Variable.Name <> "Constants" Then
                Grid.Cells(Row, 0).Value = XmlHelper.Name(Variable)
                Grid.Cells(Row, 1).Value = XmlHelper.Attribute(Variable, "array")
                Grid.Cells(Row, 2).Value = XmlHelper.Attribute(Variable, "description")
                Row += 1
            End If
        Next
    End Sub
    Private Sub PopulateConstants()
        Dim ConstantsNode As XmlNode = XmlHelper.Find(Data, "constants")
        If Not IsNothing(ConstantsNode) Then
            Dim NumConstants As Integer = XmlHelper.ChildNodes(ConstantsNode, "").Count
            Dim Lines(NumConstants) As String
            Dim Index As Integer = 0
            For Each Constant As XmlNode In XmlHelper.ChildNodes(ConstantsNode, "")
                Lines(Index) = Constant.Name + " = " + Constant.InnerText
                Index = Index + 1
            Next
            ConstantsBox.Lines = Lines
        End If
    End Sub
    Private Sub PopulateVariableListView()
        ' ----------------------------------------------
        ' Populate the variable list view box
        ' ----------------------------------------------

        If ComponentFilter.SelectedIndex >= 0 And ComponentFilter.SelectedIndex < ComponentNames.Count Then
            Windows.Forms.Cursor.Current = Cursors.WaitCursor
            VariableListView.BeginUpdate()
            VariableListView.Groups.Clear()
            VariableListView.Items.Clear()

            Dim ComponentType As String = ComponentTypes(ComponentFilter.SelectedIndex)
            Dim ComponentName As String = ComponentNames(ComponentFilter.SelectedIndex)
            Dim PropertyGroup As String = XmlHelper.Type(Data)  ' e.g. variables or events
            If PropertyGroup.ToLower = "tracker" Then
                PropertyGroup = "variables"
            End If
            If ComponentType <> "tracker" Or Data.Name = "tracker" Then
                AddVariablesToListView(ComponentName, ComponentType, PropertyGroup)
            End If

            VariableListView.EndUpdate()
            VariableListView.Columns(0).AutoResize(ColumnHeaderAutoResizeStyle.ColumnContent)
            Windows.Forms.Cursor.Current = Cursors.Default
        End If
    End Sub

    Private Sub AddVariablesToListView(ByVal ComponentName As String, ByVal ComponentType As String, ByVal PropertyGroup As String)
        Dim ModelInfo As List(Of XmlNode)
        If PropertyGroup = "variables" Then
            ModelInfo = Types.Instance.Variables(ComponentType)
        Else
            ModelInfo = Types.Instance.Events(ComponentType)
        End If

        For Each Variables As XmlNode In ModelInfo
            Dim GroupName As String = XmlHelper.Attribute(Variables, "name")
            If GroupName = "" Then
                GroupName = ComponentName + " " + PropertyGroup
            End If
            Dim NewGroup As New ListViewGroup(GroupName)
            VariableListView.Groups.Add(NewGroup)
            For Each Variable As XmlNode In XmlHelper.ChildNodes(Variables, "")
                Dim ListItem As New ListViewItem(XmlHelper.Name(Variable))
                ListItem.Group = NewGroup
                If XmlHelper.Attribute(Variable, "array") = "T" Then
                    ListItem.SubItems.Add("Yes")
                Else
                    ListItem.SubItems.Add("No")
                End If
                ListItem.SubItems.Add(XmlHelper.Attribute(Variable, "description"))
                VariableListView.Items.Add(ListItem)
            Next
        Next
    End Sub
    Private Sub SaveVariableGrid()
        ' --------------------------------------------------
        ' Save the variable grid back to the selected data.
        ' --------------------------------------------------

        ' Work out the property type from the currently selected data type by removing the last character.
        ' e.g. if current data type is 'variables' then property type is 'variable'
        Dim PropertyType As String = XmlHelper.Type(Data)
        If PropertyType.ToLower = "tracker" Then
            PropertyType = "variables"
        End If
        PropertyType = PropertyType.Remove(PropertyType.Length - 1)
        Dim NumRows As Integer = UIUtility.GridUtility.FindLastBlankCell(Grid)
        Dim DataName As String = XmlHelper.Name(Data)
        Data.RemoveAll()
        XmlHelper.SetName(Data, DataName)

        For Row As Integer = 0 To NumRows
            Dim VariableName As String = Grid.Cells(Row, 0).Text
            If (VariableName <> "") Then
                Dim Variable As XmlNode = Data.AppendChild(Data.OwnerDocument.CreateElement(PropertyType))
                XmlHelper.SetName(Variable, VariableName)
                XmlHelper.SetAttribute(Variable, "array", Grid.Cells(Row, 1).Text)
                XmlHelper.SetAttribute(Variable, "description", Grid.Cells(Row, 2).Text)
            End If
        Next
    End Sub
    Private Sub SaveConstants()
        Dim Constants As XmlNode = XmlHelper.Find(Data, "Constants")
        If Not IsNothing(Constants) Then
            Data.RemoveChild(Constants)
        End If
        For Each Line As String In ConstantsBox.Lines
            Dim PosEquals As Integer = Line.IndexOf("=")
            If PosEquals <> -1 Then
                Dim ConstantName As String = Trim(Line.Substring(0, PosEquals))
                Dim ConstantValue As String = Trim(Line.Substring(PosEquals + 1))
                XmlHelper.SetValue(Data, "Constants/" + ConstantName, ConstantValue)
            End If
        Next
    End Sub

    Private Sub GridKeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles Spread.KeyDown
        ' --------------------------------------------------
        ' If user has hit delete then delete the entire row.
        ' --------------------------------------------------
        If e.KeyCode = Keys.Delete Then
            If Grid.SelectionCount > 0 Then
                Dim Range As FarPoint.Win.Spread.Model.CellRange = Grid.GetSelection(0)
                If Range.ColumnCount = 5 Then
                    ' delete the entire rows.
                    Grid.Rows(Range.Row, Range.Row + Range.RowCount - 1).Remove()
                Else
                    ' just clear the cell contents.
                    Grid.Cells(Range.Row, Range.Column, Range.Row + Range.RowCount - 1, Range.Column + Range.ColumnCount - 1).Value = ""
                End If
            End If
        End If
    End Sub
    Public Overrides Sub OnSave()
        SaveVariableGrid()
        SaveConstants()
    End Sub
    Private Sub Grid_CellChanged(ByVal sender As System.Object, ByVal e As FarPoint.Win.Spread.SheetViewEventArgs) Handles Grid.CellChanged
        ' -------------------------------------------------------
        ' User has entered there own variable. See if we can find
        ' it in the variable list. If so, then add description
        ' and isArray to grid.
        ' -------------------------------------------------------
        If UserChange Then
            UserChange = False
            Dim LowerName As String = Grid.Cells(e.Row, e.Column).Text.ToLower
            For Each Item As ListViewItem In VariableListView.Items
                If Item.Text.ToLower = LowerName Then
                    Grid.Cells(e.Row, 0).Text = Item.Text
                    Grid.Cells(e.Row, 1).Text = Item.SubItems(2).Text
                    Grid.Cells(e.Row, 2).Text = Item.SubItems(3).Text
                End If
            Next
            'SaveVariableGrid()
            UserChange = True
        End If
    End Sub





#Region "Drag / Drop methods"

    Private Sub ListViewItemDrag(ByVal sender As Object, ByVal e As System.Windows.Forms.ItemDragEventArgs) Handles VariableListView.ItemDrag
        ' --------------------------------------------------------
        ' User is trying to initiate a drag - allow drag operation
        ' --------------------------------------------------------
        VariableListView.DoDragDrop("xx", DragDropEffects.All)
    End Sub
    Private Sub VariablesGridDragEnter(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles Spread.DragEnter
        e.Effect = DragDropEffects.Copy
    End Sub
    Private Sub VariablesGridDragOver(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles Spread.DragOver
        e.Effect = DragDropEffects.Copy
    End Sub
    Private Sub VariablesGridDragDrop(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles Spread.DragDrop
        ' --------------------------------------------------
        ' User has dropped a variable onto the variable grid
        ' --------------------------------------------------
        UserChange = False
        Dim Row As Integer = UIUtility.GridUtility.FindFirstBlankCell(Grid, 0)
        For Each SelectedItem As ListViewItem In VariableListView.SelectedItems
            Grid.Cells(Row, 0).Text = SelectedItem.Text
            Grid.Cells(Row, 1).Text = SelectedItem.SubItems(1).Text
            Grid.Cells(Row, 2).Text = SelectedItem.SubItems(2).Text
            Row += 1
        Next
        UserChange = True
        SaveVariableGrid()
    End Sub

    Private Sub VariableListView_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles VariableListView.DoubleClick
        ' ----------------------------------------------------------
        ' On a double click do exact the same thing as when you drop
        ' ----------------------------------------------------------
        UserChange = False
        Dim Row As Integer = UIUtility.GridUtility.FindFirstBlankCell(Grid, 0)
        For Each SelectedItem As ListViewItem In VariableListView.SelectedItems
            Grid.Cells(Row, 0).Text = SelectedItem.Text
            Grid.Cells(Row, 1).Text = SelectedItem.SubItems(1).Text
            Grid.Cells(Row, 2).Text = SelectedItem.SubItems(2).Text
            Row += 1
        Next
        UserChange = True
        SaveVariableGrid()
    End Sub

#End Region

    Private Function CanMoveUp() As Boolean
        If Grid.SelectionCount = 1 Then
            Dim Selection As FarPoint.Win.Spread.Model.CellRange = Grid.GetSelection(0)
            Return Selection.Row > 0
        Else
            Return False
        End If
    End Function

    Private Sub MoveUpMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MoveUpMenuItem.Click
        ' Move the 1 row from in front of current selection to one row just after the current selection
        If CanMoveUp() Then
            UserChange = False
            Dim Selection As FarPoint.Win.Spread.Model.CellRange = Grid.GetSelection(0)
            Dim FromRow As Integer = Selection.Row - 1
            Dim ToRow As Integer = Selection.Row + Selection.RowCount
            Grid.AddRows(ToRow, 1)
            For Col As Integer = 0 To Grid.ColumnCount - 1
                Grid.Cells(ToRow, Col).Text = Grid.Cells(FromRow, Col).Text
            Next
            Grid.RemoveRows(FromRow, 1)
            Grid.ClearSelection()
            Grid.AddSelection(Selection.Row - 1, Selection.Column, Selection.RowCount, Selection.ColumnCount)
            SaveVariableGrid()
            UserChange = True
        End If
    End Sub

    Private Sub MoveDownMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MoveDownMenuItem.Click
        ' Move the 1 row from in front of current selection to one row just after the current selection
        UserChange = False
        Dim Selection As FarPoint.Win.Spread.Model.CellRange = Grid.GetSelection(0)
        Dim FromRow As Integer = Selection.Row + Selection.RowCount
        Dim ToRow As Integer = Selection.Row
        Grid.AddRows(ToRow, 1)
        FromRow += 1
        For Col As Integer = 0 To Grid.ColumnCount - 1
            Grid.Cells(ToRow, Col).Text = Grid.Cells(FromRow, Col).Text
        Next
        Grid.RemoveRows(FromRow, 1)
        Grid.ClearSelection()
        Grid.AddSelection(Selection.Row + 1, Selection.Column, Selection.RowCount, Selection.ColumnCount)
        SaveVariableGrid()
        UserChange = True
    End Sub

    Private Sub ComponentFilter_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ComponentFilter.TextChanged
        PopulateVariableListView()
    End Sub

    ' --------------------------------------------------
    ' Return a list of sibling component names and types
    ' for the specified data component
    ' --------------------------------------------------
    Private Shared Sub GetSiblingComponents(ByVal Paddock As ApsimFile.Component, _
                                            ByRef ComponentNames As StringCollection, ByRef ComponentTypes As StringCollection)
        ComponentNames.Clear()
        ComponentTypes.Clear()
        ComponentNames.Add("Global")
        ComponentTypes.Add("registrations")
        For Each Sibling As ApsimFile.Component In Paddock.ChildNodes
            ComponentNames.Add(Sibling.Name)
            ComponentTypes.Add(Sibling.Type.ToLower())
        Next
    End Sub





    Private Sub OnHelpClick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HelpButton.Click
        Dim HelpText As String
        If XmlHelper.Type(Data).ToLower = "variables" Then
            HelpText = "Syntax of variables:" + vbCrLf + _
                       "  ModuleName.VariableName as Alias units kg/ha format dd/mm/yyy" + vbCrLf + _
                       "Examples:" + vbCrLf + _
                       "  yield                  - Yields from all crops" + vbCrLf + _
                       "  wheat.yield            - 'yield' for just wheat" + vbCrLf + _
                       "  wheat.yield as whtyld  - 'yield' from wheat renamed as 'whtyld'" + vbCrLf + _
                       "  yield units kg/ha      - 'yield' reported as kg/ha" + vbCrLf + _
                       "  yield format 0         - 'yield' reported with no decimal places" + vbCrLf + _
                       "  today format dd/mm/yyyy  - 'today' reported in dd/mm/yyyy format" + vbCrLf + _
                       "  sw()                   - Sum of 'sw' for all layers" + vbCrLf + _
                       "  sw(2)                  - 'sw' for the second layer" + vbCrLf + _
                       "  sw(2-4)                - 'sw' for layers 2 through to 4"
        ElseIf XmlHelper.Type(Data).ToLower = "tracker" Then
            HelpText = "Syntax of tracker variables:" + vbCrLf + _
                       "  [stat] of [VariableName] on [EventName] [from [From] to [To]] as [Alias]" + vbCrLf + _
                       "Where:" + vbCrLf + _
                       "  [stat] can be sum, value, maximum or minimum" + vbCrLf + _
                       "  [VariableName] can be any APSIM variable name" + vbCrLf + _
                       "  [EventName] is the APSIM event name to collect the value" + vbCrLf + _
                       "  [From] is the APSIM event name to start collecting the values" + vbCrLf + _
                       "  [To] is the APSIM event name to stop collecting the values" + vbCrLf + _
                       "  [Alias] is the name the variable will be known as within APSIM"
        Else
            HelpText = "Example output frequencies:" + vbCrLf + _
                       "  Daily       - reporting will be done daily" + vbCrLf + _
                       "  end_week    - reporting will be done at the end of each week" + vbCrLf + _
                       "  end_month   - reporting will be done at the end of each month" + vbCrLf + _
                       "  end_year    - reporting will be done at the end of each year" + vbCrLf + _
                       "  harvesting  - reporting will be done at harvest of a crop"
        End If
        MessageBox.Show(HelpText, "", MessageBoxButtons.OK, MessageBoxIcon.Information)
    End Sub
End Class
