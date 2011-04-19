
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
   Friend WithEvents ConstantsPanel As System.Windows.Forms.Panel
   Friend WithEvents GridLabel As System.Windows.Forms.Label
   Friend WithEvents ConstantsLabel As System.Windows.Forms.Label
   Friend WithEvents DictionaryLabel As System.Windows.Forms.Label
   Friend WithEvents HelpButton As System.Windows.Forms.Button
   Friend WithEvents DeleteVariablesMenuItem As System.Windows.Forms.ToolStripMenuItem
   Friend WithEvents Grid As UIBits.EnhancedGrid
   Friend WithEvents HelpPanel As System.Windows.Forms.Panel
   Friend WithEvents BottomPanel As System.Windows.Forms.Panel
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
   Friend WithEvents GridPanel As System.Windows.Forms.Panel
   Friend WithEvents FpSpread1 As FarPoint.Win.Spread.FpSpread
   Friend WithEvents GridContextMenu As System.Windows.Forms.ContextMenuStrip
   Friend WithEvents MoveUpMenuItem As System.Windows.Forms.ToolStripMenuItem
   Friend WithEvents MoveDownMenuItem As System.Windows.Forms.ToolStripMenuItem
   Friend WithEvents FilterPanel As System.Windows.Forms.Panel
   Friend WithEvents VariableListView As System.Windows.Forms.ListView
   Friend WithEvents ColumnHeader1 As System.Windows.Forms.ColumnHeader
   Friend WithEvents ColumnHeader4 As System.Windows.Forms.ColumnHeader
   Friend WithEvents ColumnHeader3 As System.Windows.Forms.ColumnHeader
   Friend WithEvents Label1 As System.Windows.Forms.Label
   Friend WithEvents ComponentFilter As System.Windows.Forms.ComboBox

   <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
      Me.components = New System.ComponentModel.Container
      Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(OutputFileDescUI))
      Me.OpenFileDialog = New System.Windows.Forms.OpenFileDialog
      Me.GridPanel = New System.Windows.Forms.Panel
      Me.Grid = New UIBits.EnhancedGrid
      Me.HelpPanel = New System.Windows.Forms.Panel
      Me.GridLabel = New System.Windows.Forms.Label
      Me.HelpButton = New System.Windows.Forms.Button
      Me.Splitter1 = New System.Windows.Forms.Splitter
      Me.Splitter2 = New System.Windows.Forms.Splitter
      Me.FilterPanel = New System.Windows.Forms.Panel
      Me.DictionaryLabel = New System.Windows.Forms.Label
      Me.VariableListView = New System.Windows.Forms.ListView
      Me.ColumnHeader1 = New System.Windows.Forms.ColumnHeader
      Me.ColumnHeader4 = New System.Windows.Forms.ColumnHeader
      Me.ColumnHeader3 = New System.Windows.Forms.ColumnHeader
      Me.Label1 = New System.Windows.Forms.Label
      Me.ComponentFilter = New System.Windows.Forms.ComboBox
      Me.ConstantsPanel = New System.Windows.Forms.Panel
      Me.ConstantsBox = New System.Windows.Forms.TextBox
      Me.ConstantsLabel = New System.Windows.Forms.Label
      Me.GridContextMenu = New System.Windows.Forms.ContextMenuStrip(Me.components)
      Me.MoveUpMenuItem = New System.Windows.Forms.ToolStripMenuItem
      Me.MoveDownMenuItem = New System.Windows.Forms.ToolStripMenuItem
      Me.DeleteVariablesMenuItem = New System.Windows.Forms.ToolStripMenuItem
      Me.BottomPanel = New System.Windows.Forms.Panel
      Me.GridPanel.SuspendLayout()
      CType(Me.Grid, System.ComponentModel.ISupportInitialize).BeginInit()
      Me.HelpPanel.SuspendLayout()
      Me.FilterPanel.SuspendLayout()
      Me.ConstantsPanel.SuspendLayout()
      Me.GridContextMenu.SuspendLayout()
      Me.BottomPanel.SuspendLayout()
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
      'GridPanel
      '
      Me.GridPanel.Controls.Add(Me.Grid)
      Me.GridPanel.Controls.Add(Me.HelpPanel)
      Me.GridPanel.Dock = System.Windows.Forms.DockStyle.Fill
      Me.GridPanel.Location = New System.Drawing.Point(0, 0)
      Me.GridPanel.Name = "GridPanel"
      Me.GridPanel.Size = New System.Drawing.Size(353, 434)
      Me.GridPanel.TabIndex = 11
      '
      'Grid
      '
      Me.Grid.AllowDrop = True
      Me.Grid.BorderStyle = System.Windows.Forms.BorderStyle.None
      Me.Grid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
      Me.Grid.ColumnHeadersVisible = False
      Me.Grid.DataSourceTable = Nothing
      Me.Grid.Dock = System.Windows.Forms.DockStyle.Fill
      Me.Grid.Location = New System.Drawing.Point(0, 30)
      Me.Grid.Name = "Grid"
      Me.Grid.RowHeadersVisible = False
      Me.Grid.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect
      Me.Grid.Size = New System.Drawing.Size(353, 404)
      Me.Grid.TabIndex = 27
      '
      'HelpPanel
      '
      Me.HelpPanel.Controls.Add(Me.GridLabel)
      Me.HelpPanel.Controls.Add(Me.HelpButton)
      Me.HelpPanel.Dock = System.Windows.Forms.DockStyle.Top
      Me.HelpPanel.Location = New System.Drawing.Point(0, 0)
      Me.HelpPanel.Name = "HelpPanel"
      Me.HelpPanel.Size = New System.Drawing.Size(353, 30)
      Me.HelpPanel.TabIndex = 27
      '
      'GridLabel
      '
      Me.GridLabel.BackColor = System.Drawing.SystemColors.ActiveCaption
      Me.GridLabel.Dock = System.Windows.Forms.DockStyle.Fill
      Me.GridLabel.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
      Me.GridLabel.ForeColor = System.Drawing.SystemColors.ActiveCaptionText
      Me.GridLabel.Location = New System.Drawing.Point(30, 0)
      Me.GridLabel.Name = "GridLabel"
      Me.GridLabel.Size = New System.Drawing.Size(323, 30)
      Me.GridLabel.TabIndex = 25
      Me.GridLabel.Text = "Output file columns:"
      Me.GridLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
      '
      'HelpButton
      '
      Me.HelpButton.AutoSize = True
      Me.HelpButton.Dock = System.Windows.Forms.DockStyle.Left
      Me.HelpButton.Image = CType(resources.GetObject("HelpButton.Image"), System.Drawing.Image)
      Me.HelpButton.Location = New System.Drawing.Point(0, 0)
      Me.HelpButton.Name = "HelpButton"
      Me.HelpButton.Size = New System.Drawing.Size(30, 30)
      Me.HelpButton.TabIndex = 26
      Me.HelpButton.UseVisualStyleBackColor = True
      '
      'Splitter1
      '
      Me.Splitter1.Dock = System.Windows.Forms.DockStyle.Top
      Me.Splitter1.Location = New System.Drawing.Point(0, 87)
      Me.Splitter1.Name = "Splitter1"
      Me.Splitter1.Size = New System.Drawing.Size(753, 3)
      Me.Splitter1.TabIndex = 22
      Me.Splitter1.TabStop = False
      '
      'Splitter2
      '
      Me.Splitter2.Dock = System.Windows.Forms.DockStyle.Right
      Me.Splitter2.Location = New System.Drawing.Point(353, 0)
      Me.Splitter2.Name = "Splitter2"
      Me.Splitter2.Size = New System.Drawing.Size(3, 434)
      Me.Splitter2.TabIndex = 23
      Me.Splitter2.TabStop = False
      '
      'FilterPanel
      '
      Me.FilterPanel.Controls.Add(Me.DictionaryLabel)
      Me.FilterPanel.Controls.Add(Me.VariableListView)
      Me.FilterPanel.Controls.Add(Me.Label1)
      Me.FilterPanel.Controls.Add(Me.ComponentFilter)
      Me.FilterPanel.Dock = System.Windows.Forms.DockStyle.Right
      Me.FilterPanel.Location = New System.Drawing.Point(356, 0)
      Me.FilterPanel.Name = "FilterPanel"
      Me.FilterPanel.Size = New System.Drawing.Size(397, 434)
      Me.FilterPanel.TabIndex = 18
      '
      'DictionaryLabel
      '
      Me.DictionaryLabel.BackColor = System.Drawing.SystemColors.ActiveCaption
      Me.DictionaryLabel.Dock = System.Windows.Forms.DockStyle.Top
      Me.DictionaryLabel.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
      Me.DictionaryLabel.ForeColor = System.Drawing.SystemColors.ActiveCaptionText
      Me.DictionaryLabel.Location = New System.Drawing.Point(0, 0)
      Me.DictionaryLabel.Name = "DictionaryLabel"
      Me.DictionaryLabel.Size = New System.Drawing.Size(397, 30)
      Me.DictionaryLabel.TabIndex = 22
      Me.DictionaryLabel.Text = "Variables to drag onto grid:"
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
      Me.VariableListView.Size = New System.Drawing.Size(374, 361)
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
      Me.ComponentFilter.Size = New System.Drawing.Size(155, 21)
      Me.ComponentFilter.TabIndex = 18
      '
      'ConstantsPanel
      '
      Me.ConstantsPanel.Controls.Add(Me.ConstantsBox)
      Me.ConstantsPanel.Controls.Add(Me.ConstantsLabel)
      Me.ConstantsPanel.Dock = System.Windows.Forms.DockStyle.Top
      Me.ConstantsPanel.Location = New System.Drawing.Point(0, 16)
      Me.ConstantsPanel.Name = "ConstantsPanel"
      Me.ConstantsPanel.Size = New System.Drawing.Size(753, 71)
      Me.ConstantsPanel.TabIndex = 24
      '
      'ConstantsBox
      '
      Me.ConstantsBox.BorderStyle = System.Windows.Forms.BorderStyle.None
      Me.ConstantsBox.Dock = System.Windows.Forms.DockStyle.Fill
      Me.ConstantsBox.Location = New System.Drawing.Point(0, 30)
      Me.ConstantsBox.Multiline = True
      Me.ConstantsBox.Name = "ConstantsBox"
      Me.ConstantsBox.Size = New System.Drawing.Size(753, 41)
      Me.ConstantsBox.TabIndex = 19
      '
      'ConstantsLabel
      '
      Me.ConstantsLabel.BackColor = System.Drawing.SystemColors.ActiveCaption
      Me.ConstantsLabel.Dock = System.Windows.Forms.DockStyle.Top
      Me.ConstantsLabel.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
      Me.ConstantsLabel.ForeColor = System.Drawing.SystemColors.ActiveCaptionText
      Me.ConstantsLabel.Location = New System.Drawing.Point(0, 0)
      Me.ConstantsLabel.Name = "ConstantsLabel"
      Me.ConstantsLabel.Size = New System.Drawing.Size(753, 30)
      Me.ConstantsLabel.TabIndex = 20
      Me.ConstantsLabel.Text = "Constants to put in top of output file:"
      Me.ConstantsLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
      '
      'GridContextMenu
      '
      Me.GridContextMenu.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.MoveUpMenuItem, Me.MoveDownMenuItem, Me.DeleteVariablesMenuItem})
      Me.GridContextMenu.Name = "ContextMenu"
      Me.GridContextMenu.Size = New System.Drawing.Size(252, 70)
      '
      'MoveUpMenuItem
      '
      Me.MoveUpMenuItem.Name = "MoveUpMenuItem"
      Me.MoveUpMenuItem.ShortcutKeys = CType((System.Windows.Forms.Keys.Control Or System.Windows.Forms.Keys.Up), System.Windows.Forms.Keys)
      Me.MoveUpMenuItem.Size = New System.Drawing.Size(251, 22)
      Me.MoveUpMenuItem.Text = "Move variables &up"
      '
      'MoveDownMenuItem
      '
      Me.MoveDownMenuItem.Name = "MoveDownMenuItem"
      Me.MoveDownMenuItem.ShortcutKeys = CType((System.Windows.Forms.Keys.Control Or System.Windows.Forms.Keys.Down), System.Windows.Forms.Keys)
      Me.MoveDownMenuItem.Size = New System.Drawing.Size(251, 22)
      Me.MoveDownMenuItem.Text = "Move variables &down"
      '
      'DeleteVariablesMenuItem
      '
      Me.DeleteVariablesMenuItem.Name = "DeleteVariablesMenuItem"
      Me.DeleteVariablesMenuItem.Size = New System.Drawing.Size(251, 22)
      Me.DeleteVariablesMenuItem.Text = "Delete variables"
      '
      'BottomPanel
      '
      Me.BottomPanel.Controls.Add(Me.GridPanel)
      Me.BottomPanel.Controls.Add(Me.Splitter2)
      Me.BottomPanel.Controls.Add(Me.FilterPanel)
      Me.BottomPanel.Dock = System.Windows.Forms.DockStyle.Fill
      Me.BottomPanel.Location = New System.Drawing.Point(0, 90)
      Me.BottomPanel.Name = "BottomPanel"
      Me.BottomPanel.Size = New System.Drawing.Size(753, 434)
      Me.BottomPanel.TabIndex = 28
      '
      'OutputFileDescUI
      '
      Me.Controls.Add(Me.BottomPanel)
      Me.Controls.Add(Me.Splitter1)
      Me.Controls.Add(Me.ConstantsPanel)
      Me.Name = "OutputFileDescUI"
      Me.Size = New System.Drawing.Size(753, 524)
      Me.Controls.SetChildIndex(Me.MyHelpLabel, 0)
      Me.Controls.SetChildIndex(Me.ConstantsPanel, 0)
      Me.Controls.SetChildIndex(Me.Splitter1, 0)
      Me.Controls.SetChildIndex(Me.BottomPanel, 0)
      Me.GridPanel.ResumeLayout(False)
      CType(Me.Grid, System.ComponentModel.ISupportInitialize).EndInit()
      Me.HelpPanel.ResumeLayout(False)
      Me.HelpPanel.PerformLayout()
      Me.FilterPanel.ResumeLayout(False)
      Me.FilterPanel.PerformLayout()
      Me.ConstantsPanel.ResumeLayout(False)
      Me.ConstantsPanel.PerformLayout()
      Me.GridContextMenu.ResumeLayout(False)
      Me.BottomPanel.ResumeLayout(False)
      Me.ResumeLayout(False)

   End Sub

#End Region

   Overrides Sub OnRefresh()
      ' ----------------------------------
      ' Refresh the variable grid
      ' ----------------------------------

      Dim Table As New DataTable
      If XmlHelper.Type(Data).ToLower = "variables" Then
         GridLabel.Text = "Output file columns:"
         DictionaryLabel.Text = "Variables to drag onto grid:"
         Table.Columns.Add("Variable name", System.Type.GetType("System.String"))
      ElseIf XmlHelper.Type(Data).ToLower = "tracker" Then
         GridLabel.Text = "Tracker variables:"
         DictionaryLabel.Text = "Example tracker variables - drag to the grid."
         Table.Columns.Add("Tracker variable", System.Type.GetType("System.String"))
      Else
         GridLabel.Text = "Output file frequencies:"
         DictionaryLabel.Text = "Frequency list - drag to the grid."
         Table.Columns.Add("Output frequency", System.Type.GetType("System.String"))
      End If

      ' Fill data table.
      For Each Variable As XmlNode In XmlHelper.ChildNodes(Data, "")
         If Variable.Name <> "Constants" Then
            Dim NewRow As DataRow = Table.NewRow()
            NewRow(0) = XmlHelper.Name(Variable)
            Table.Rows.Add(NewRow)
         End If
      Next

      ' Give data table to grid.
      Grid.DataSourceTable = Table

      ' We want to find the component that is a child of our paddock.
      Dim Paddock As ApsimFile.Component = Controller.ApsimData.Find(NodePath).FindContainingPaddock()
      GetSiblingComponents(Paddock, ComponentNames, ComponentTypes)

      UserChange = False
      PopulateComponentFilter()
      PopulateVariableListView()
      UserChange = True

      If XmlHelper.Type(Data).ToLower <> "variables" Then
         VariableListView.Columns(1).Width = 0
         ConstantsPanel.Visible = False

      Else
         VariableListView.Columns(1).Width = 45
         ConstantsPanel.Visible = True
         PopulateConstants()
      End If

      Grid.Columns(0).AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill
      Grid.Columns(0).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft
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
         If ComponentFilter.Items.Count > 0 Then
            ComponentFilter.SelectedIndex = 0
         End If
      End If

   End Sub
    Private Sub PopulateConstants()
        'Clear out all the old stuff because these UI's are reused by other nodes of the same type.
        ConstantsBox.Clear()
        'Fill it in with the new stuff from this node.
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
      Dim ModelInfo As List(Of Types.MetaDataInfo)
      If PropertyGroup = "variables" Then
         ModelInfo = Types.Instance.Variables(ComponentType)
      Else
         ModelInfo = Types.Instance.Events(ComponentType)
      End If

      Dim GroupName As String = ComponentName
      If GroupName = "" Then
         GroupName = ComponentName + " " + PropertyGroup
      End If
      Dim NewGroup As New ListViewGroup(GroupName)

      For Each Variable As Types.MetaDataInfo In ModelInfo
         VariableListView.Groups.Add(NewGroup)
         Dim ListItem As New ListViewItem(Variable.Name)
         ListItem.Group = NewGroup
         If Variable.IsArray Then
            ListItem.SubItems.Add("Yes")
         Else
            ListItem.SubItems.Add("No")
         End If
         ListItem.SubItems.Add(Variable.Description)
         VariableListView.Items.Add(ListItem)
      Next
   End Sub
   Public Overrides Sub OnSave()
      ' --------------------------------------------------
      ' Save the variable grid back to the selected data.
      ' --------------------------------------------------

      ' Work out the property type from the currently selected data type by removing the last character.
      ' e.g. if current data type is 'variables' then property type is 'variable'
      Dim PropertyType As String = XmlHelper.Type(Data)
      If PropertyType.ToLower = "tracker" Then
         PropertyType = "variables"
      End If
      PropertyType = PropertyType.Remove(PropertyType.Length - 1) ' Turn from plural to singular.

      Dim VariableNames() As String = DataTableUtility.GetColumnAsStrings(Grid.DataSourceTable, Grid.DataSourceTable.Columns(0).ColumnName)

      Data.RemoveAll()
      Dim DataName As String = XmlHelper.Name(Data)
      XmlHelper.SetName(Data, DataName)
      For Each VariableName As String In VariableNames
         If VariableName <> "" Then
            Dim Variable As XmlNode = Data.AppendChild(Data.OwnerDocument.CreateElement(PropertyType))
            XmlHelper.SetName(Variable, VariableName)
         End If
      Next

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


   Private Sub AddVariablesToGrid(ByVal VariableNames As ListView.SelectedListViewItemCollection)
      UserChange = False
      Dim Table As DataTable = Grid.DataSourceTable

      For Each SelectedItem As ListViewItem In VariableNames
         ' Go look for a blank cell.
         Dim Row As Integer
         For Row = 0 To Table.Rows.Count - 1
            If IsDBNull(Table.Rows(Row)(0)) OrElse Table.Rows(Row)(0) = "" Then
               Exit For
            End If
         Next
         If Row = Table.Rows.Count Then
            Dim NewRow As DataRow = Grid.DataSourceTable.NewRow
            NewRow(0) = SelectedItem.Text
            Table.Rows.Add(NewRow)
         Else
            Table.Rows(Row)(0) = SelectedItem.Text
         End If
      Next
      Grid.PopulateGrid()
      Grid.Columns(0).AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill
      Grid.Columns(0).DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft
      UserChange = True
   End Sub


#Region "Drag / Drop methods"

   Private Sub ListViewItemDrag(ByVal sender As Object, ByVal e As System.Windows.Forms.ItemDragEventArgs) Handles VariableListView.ItemDrag
      ' --------------------------------------------------------
      ' User is trying to initiate a drag - allow drag operation
      ' --------------------------------------------------------
      VariableListView.DoDragDrop("xx", DragDropEffects.All)
   End Sub
   Private Sub VariablesGridDragEnter(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles Grid.DragEnter
      e.Effect = DragDropEffects.Copy
   End Sub
   Private Sub VariablesGridDragOver(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles Grid.DragOver
      e.Effect = DragDropEffects.Copy
   End Sub
   Private Sub VariablesGridDragDrop(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles Grid.DragDrop
      ' --------------------------------------------------
      ' User has dropped a variable onto the variable grid
      ' --------------------------------------------------
      AddVariablesToGrid(VariableListView.SelectedItems)
   End Sub

   Private Sub VariableListView_DoubleClick(ByVal sender As Object, ByVal e As System.EventArgs) Handles VariableListView.DoubleClick
      ' ----------------------------------------------------------
      ' On a double click do exact the same thing as when you drop
      ' ----------------------------------------------------------
      AddVariablesToGrid(VariableListView.SelectedItems)
   End Sub

#End Region

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
      If Not IsNothing(Paddock) Then
         If Not IsNothing(Paddock.Parent) AndAlso Not IsNothing(Paddock.Parent.Parent) Then
            GetSiblingComponents(Paddock.Parent, ComponentNames, ComponentTypes)
         End If
         For Each Sibling As ApsimFile.Component In Paddock.ChildNodes
            If Sibling.Type.ToLower <> "simulation" And Sibling.Type.ToLower <> "graph" Then
               ComponentNames.Add(Sibling.Name)
               ComponentTypes.Add(Sibling.Type.ToLower())
            End If
         Next
      End If
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
