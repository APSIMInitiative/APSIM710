

Imports System.Collections.Specialized
Imports System.IO
Imports System.Windows.Forms
Imports System.xml

Imports CSGeneral


Public Class ExplorerUI
    Inherits UserControl
    Private UIs As New ArrayList
    Private UITypes As New StringCollection
    Private CurrentUIIndex As Integer = -1
    Private Controller As BaseController    'this is very important. The base controller controls all the actions and events in ApsimUI. There is only one base controller variable for the entire ApsimUI and it gets passed around because it is needed to deal with clicks etc.


#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()
        InitializeComponent()
    End Sub
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
    Friend WithEvents Splitter As System.Windows.Forms.Splitter
    Friend WithEvents UIPanel As System.Windows.Forms.Panel
    Friend WithEvents DataTree As Controllers.DataTree
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Me.Splitter = New System.Windows.Forms.Splitter
        Me.UIPanel = New System.Windows.Forms.Panel
        Me.DataTree = New Controllers.DataTree
        Me.SuspendLayout()
        '
        'Splitter
        '
        Me.Splitter.BackColor = System.Drawing.SystemColors.ControlLight
        Me.Splitter.Location = New System.Drawing.Point(256, 0)
        Me.Splitter.Name = "Splitter"
        Me.Splitter.Size = New System.Drawing.Size(5, 828)
        Me.Splitter.TabIndex = 4
        Me.Splitter.TabStop = False
        '
        'UIPanel
        '
        Me.UIPanel.BackColor = System.Drawing.SystemColors.Window
        Me.UIPanel.Dock = System.Windows.Forms.DockStyle.Fill
        Me.UIPanel.Location = New System.Drawing.Point(261, 0)
        Me.UIPanel.Name = "UIPanel"
        Me.UIPanel.Size = New System.Drawing.Size(759, 828)
        Me.UIPanel.TabIndex = 5
        '
        'DataTree
        '
        Me.DataTree.AllowDrop = True
        Me.DataTree.BackColor = System.Drawing.SystemColors.Window
        Me.DataTree.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.DataTree.Dock = System.Windows.Forms.DockStyle.Left
        Me.DataTree.Location = New System.Drawing.Point(0, 0)
        Me.DataTree.Name = "DataTree"
        Me.DataTree.Size = New System.Drawing.Size(256, 828)
        Me.DataTree.TabIndex = 3
        '
        'ExplorerUI
        '
        Me.Controls.Add(Me.UIPanel)
        Me.Controls.Add(Me.Splitter)
        Me.Controls.Add(Me.DataTree)
        Me.Name = "ExplorerUI"
        Me.Size = New System.Drawing.Size(1020, 828)
        Me.ResumeLayout(False)

    End Sub

#End Region

    Public Overloads Sub OnLoad(ByVal Controller As BaseController)
        Me.Controller = Controller          'set the controller to "the" base controller for ApsimUI
        DataTree.OnLoad(Controller)
        AddHandler Controller.SelectionChangedEvent, AddressOf OnSelectionChanged   'ExplorerUI will handle a "Selection Changed" event (see OnSelectionChanged method for how)
        AddHandler Controller.ApsimData.BeforeSave, AddressOf OnBeforeSave          'ExplorerUI will handle a "Before Save" event (see OnBeforeSave method for how)
    End Sub


    Public Sub ExpandAll()
        DataTree.ExpandAll()
    End Sub
    Public Sub CollapseAll()
        DataTree.CollapseAll()
    End Sub
    Private Sub ShowUI()
        ' -------------------------------------------------
        ' Create and show a specific UI depending on the
        ' currently selected data
        ' -------------------------------------------------
        Dim SelectedData As ApsimFile.Component = Controller.ApsimData.Find(Controller.SelectedPath)
        If CurrentUIIndex = -1 OrElse UITypes(CurrentUIIndex) <> SelectedData.Type Then
            CloseUI()
            CurrentUIIndex = UITypes.IndexOf(SelectedData.Type)
            If CurrentUIIndex = -1 Then
                Dim View As BaseView = Controller.CreateUI(SelectedData.Type)
                If Not IsNothing(View) Then
                    UIs.Add(View)
                    UITypes.Add(SelectedData.Type)
                    CurrentUIIndex = UIs.Count - 1
                End If
            End If
        Else
            SaveCurrentView()
        End If
        If CurrentUIIndex <> -1 Then
            Try
                Dim View As BaseView = UIs(CurrentUIIndex)
                View.OnLoad(Controller, Controller.SelectedPath, Controller.Selection.Contents)
                View.Parent = UIPanel
                View.Dock = DockStyle.Fill
                View.Show()
                View.OnRefresh()
            Catch ex As Exception
                MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
        End If
    End Sub
    Public Sub CloseUI()
        ' -------------------------------------------------
        ' Close the current UI
        ' -------------------------------------------------
        If CurrentUIIndex <> -1 Then
            Dim View As BaseView = UIs(CurrentUIIndex)
            SaveCurrentView()
            View.OnClose()
            UIPanel.Controls.Remove(View)
            CurrentUIIndex = -1
        End If
    End Sub
    Public Sub SaveCurrentView()
        ' -----------------------------------------------------
        ' Tell current view to save.
        ' -----------------------------------------------------
        If CurrentUIIndex <> -1 Then
            Dim View As BaseView = UIs(CurrentUIIndex)
            Dim Comp As ApsimFile.Component = Controller.ApsimData.Find(View.NodePath)
            If Not IsNothing(Comp) Then
                View.OnSave()
                Comp.Contents = View.GetData()
            End If
        End If
    End Sub
    Public Sub RefreshCurrentView()
        If CurrentUIIndex <> -1 Then
            Dim View As BaseView = UIs(CurrentUIIndex)
            View.OnLoad(Controller, Controller.SelectedPath, Controller.Selection.Contents)
            View.OnRefresh()
        End If
    End Sub
    Public ReadOnly Property CurrentView() As BaseView
        Get
            If CurrentUIIndex <> -1 Then
                Return UIs(CurrentUIIndex)
            Else
                Return Nothing
            End If
        End Get
    End Property
    Private Sub OnBeforeSave()
        ' -----------------------------------------------------
        ' User is about to do a save.
        ' -----------------------------------------------------
        If Controller.SelectedPaths.Count = 1 Then
            SaveCurrentView()
        End If
    End Sub
    Private Sub OnSelectionChanged(ByVal OldSelections As StringCollection, ByVal NewSelections As StringCollection)
        ' -----------------------------------------------------
        ' User has selected a node - update user interface
        ' -----------------------------------------------------
        Visible = True                                              'make the ExplorerUI visible
        Dim SavedCursor As Cursor = Windows.Forms.Cursor.Current    'store the current cursor object (usually an arrow)
        Windows.Forms.Cursor.Current = Cursors.WaitCursor           'set the cursor object to the default cursor object used for waiting (usually an hourglass) 

        'If there is only 1 node that is selected
        If Controller.SelectedPaths.Count = 1 Then
            ShowUI()        'show the corresponding UI for that node in the panel to the right of the tree

            'If there are multiple nodes selected
        Else
            CloseUI()       'show no UI in the panel to the right of the tree
        End If
        Windows.Forms.Cursor.Current = SavedCursor                  'restore the cursor object to what it was before the wait cursor.
    End Sub

End Class
