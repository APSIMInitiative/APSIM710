

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
    Friend WithEvents pnlHost As System.Windows.Forms.Panel
    Friend WithEvents Splitter1 As System.Windows.Forms.Splitter
    Friend WithEvents Title As System.Windows.Forms.Panel
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents Panel1 As System.Windows.Forms.Panel
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents pnlPlaceHolder As System.Windows.Forms.Panel
    Friend WithEvents pnlDisplayArea As System.Windows.Forms.Panel
    Friend WithEvents FactorTree As Controllers.FactorTree
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
        Me.pnlDisplayArea = New System.Windows.Forms.Panel
        Me.pnlHost = New System.Windows.Forms.Panel
        Me.Panel1 = New System.Windows.Forms.Panel
        Me.Splitter1 = New System.Windows.Forms.Splitter
        Me.pnlPlaceHolder = New System.Windows.Forms.Panel
        Me.Title = New System.Windows.Forms.Panel
        Me.Label1 = New System.Windows.Forms.Label
        Me.UIPanel = New System.Windows.Forms.Panel
        Me.Label2 = New System.Windows.Forms.Label
        Me.FactorTree = New Controllers.FactorTree(Me.components)
        Me.DataTree = New Controllers.DataTree
        Me.pnlDisplayArea.SuspendLayout()
        Me.pnlHost.SuspendLayout()
        Me.pnlPlaceHolder.SuspendLayout()
        Me.Title.SuspendLayout()
        Me.SuspendLayout()
        '
        'Splitter
        '
        Me.Splitter.BackColor = System.Drawing.SystemColors.ControlLight
        Me.Splitter.Location = New System.Drawing.Point(225, 0)
        Me.Splitter.Name = "Splitter"
        Me.Splitter.Size = New System.Drawing.Size(5, 828)
        Me.Splitter.TabIndex = 4
        Me.Splitter.TabStop = False
        '
        'pnlDisplayArea
        '
        Me.pnlDisplayArea.Controls.Add(Me.pnlHost)
        Me.pnlDisplayArea.Controls.Add(Me.UIPanel)
        Me.pnlDisplayArea.Dock = System.Windows.Forms.DockStyle.Fill
        Me.pnlDisplayArea.Location = New System.Drawing.Point(230, 0)
        Me.pnlDisplayArea.Name = "pnlDisplayArea"
        Me.pnlDisplayArea.Size = New System.Drawing.Size(790, 828)
        Me.pnlDisplayArea.TabIndex = 7
        '
        'pnlHost
        '
        Me.pnlHost.BackColor = System.Drawing.SystemColors.Window
        Me.pnlHost.Controls.Add(Me.Panel1)
        Me.pnlHost.Controls.Add(Me.Splitter1)
        Me.pnlHost.Controls.Add(Me.pnlPlaceHolder)
        Me.pnlHost.Controls.Add(Me.Title)
        Me.pnlHost.Dock = System.Windows.Forms.DockStyle.Left
        Me.pnlHost.Location = New System.Drawing.Point(0, 0)
        Me.pnlHost.Name = "pnlHost"
        Me.pnlHost.Size = New System.Drawing.Size(451, 828)
        Me.pnlHost.TabIndex = 6
        '
        'Panel1
        '
        Me.Panel1.BackColor = System.Drawing.SystemColors.Window
        Me.Panel1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.Panel1.ForeColor = System.Drawing.SystemColors.ControlText
        Me.Panel1.Location = New System.Drawing.Point(204, 20)
        Me.Panel1.Name = "Panel1"
        Me.Panel1.Padding = New System.Windows.Forms.Padding(4)
        Me.Panel1.Size = New System.Drawing.Size(247, 808)
        Me.Panel1.TabIndex = 8
        '
        'Splitter1
        '
        Me.Splitter1.BackColor = System.Drawing.SystemColors.ControlLight
        Me.Splitter1.Location = New System.Drawing.Point(199, 20)
        Me.Splitter1.Name = "Splitter1"
        Me.Splitter1.Size = New System.Drawing.Size(5, 808)
        Me.Splitter1.TabIndex = 5
        Me.Splitter1.TabStop = False
        '
        'pnlPlaceHolder
        '
        Me.pnlPlaceHolder.BackColor = System.Drawing.SystemColors.Window
        Me.pnlPlaceHolder.Controls.Add(Me.FactorTree)
        Me.pnlPlaceHolder.Dock = System.Windows.Forms.DockStyle.Left
        Me.pnlPlaceHolder.ForeColor = System.Drawing.SystemColors.ActiveCaptionText
        Me.pnlPlaceHolder.Location = New System.Drawing.Point(0, 20)
        Me.pnlPlaceHolder.Name = "pnlPlaceHolder"
        Me.pnlPlaceHolder.Padding = New System.Windows.Forms.Padding(4)
        Me.pnlPlaceHolder.Size = New System.Drawing.Size(199, 808)
        Me.pnlPlaceHolder.TabIndex = 9
        '
        'Title
        '
        Me.Title.BackColor = System.Drawing.SystemColors.ControlDarkDark
        Me.Title.Controls.Add(Me.Label1)
        Me.Title.Dock = System.Windows.Forms.DockStyle.Top
        Me.Title.ForeColor = System.Drawing.SystemColors.ControlText
        Me.Title.Location = New System.Drawing.Point(0, 0)
        Me.Title.Name = "Title"
        Me.Title.Size = New System.Drawing.Size(451, 20)
        Me.Title.TabIndex = 6
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label1.ForeColor = System.Drawing.SystemColors.ActiveCaptionText
        Me.Label1.Location = New System.Drawing.Point(4, 1)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(141, 16)
        Me.Label1.TabIndex = 0
        Me.Label1.Text = "Factorial Configuration"
        '
        'UIPanel
        '
        Me.UIPanel.BackColor = System.Drawing.SystemColors.Window
        Me.UIPanel.Location = New System.Drawing.Point(469, 3)
        Me.UIPanel.Name = "UIPanel"
        Me.UIPanel.Size = New System.Drawing.Size(302, 825)
        Me.UIPanel.TabIndex = 5
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label2.Location = New System.Drawing.Point(4, 1)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(141, 16)
        Me.Label2.TabIndex = 0
        Me.Label2.Text = "Factorial Configuration"
        '
        'FactorTree
        '
        Me.FactorTree.AllowDrop = True
        Me.FactorTree.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.FactorTree.Dock = System.Windows.Forms.DockStyle.Fill
        Me.FactorTree.HideSelection = False
        Me.FactorTree.Location = New System.Drawing.Point(4, 4)
        Me.FactorTree.Name = "FactorTree"
        Me.FactorTree.Size = New System.Drawing.Size(191, 800)
        Me.FactorTree.TabIndex = 0
        '
        'DataTree
        '
        Me.DataTree.AllowDrop = True
        Me.DataTree.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.DataTree.Dock = System.Windows.Forms.DockStyle.Left
        Me.DataTree.Location = New System.Drawing.Point(0, 0)
        Me.DataTree.Name = "DataTree"
        Me.DataTree.Size = New System.Drawing.Size(225, 828)
        Me.DataTree.TabIndex = 3
        '
        'ExplorerUI
        '
        Me.Controls.Add(Me.pnlDisplayArea)
        Me.Controls.Add(Me.Splitter)
        Me.Controls.Add(Me.DataTree)
        Me.Name = "ExplorerUI"
        Me.Size = New System.Drawing.Size(1020, 828)
        Me.pnlDisplayArea.ResumeLayout(False)
        Me.pnlHost.ResumeLayout(False)
        Me.pnlPlaceHolder.ResumeLayout(False)
        Me.Title.ResumeLayout(False)
        Me.Title.PerformLayout()
        Me.ResumeLayout(False)

    End Sub

#End Region

    Public Overloads Sub OnLoad(ByVal Controller As BaseController)
        Me.Controller = Controller          'set the controller to "the" base controller for ApsimUI
        DataTree.OnLoad(Controller)
        FactorTree.OnLoad(Controller)
        AddHandler Controller.SelectionChangedEvent, AddressOf OnSelectionChanged   'ExplorerUI will handle a "Selection Changed" event (see OnSelectionChanged method for how)
        AddHandler Controller.ApsimData.BeforeSave, AddressOf OnBeforeSave          'ExplorerUI will handle a "Before Save" event (see OnBeforeSave method for how)

        AddHandler Controller.FactorialSelectionChangedEvent, AddressOf OnFactorialSelectionChanged   'ExplorerUI will handle a "Factorial Selection Changed" event (see OnFactorialSelectionChanged method for how)
        'initialise panels to avoid redrawing problem
        UIPanel.Dock = DockStyle.Fill
        pnlHost.Dock = DockStyle.Fill
        pnlHost.Visible = False

    End Sub


    Public Sub ExpandAll()
        DataTree.ExpandAll()
    End Sub
    Public Sub CollapseAll()
        DataTree.CollapseAll()
    End Sub
    Private Sub ShowUI(ByVal SelectedPath As String, ByVal SelectedComponent As ApsimFile.Component)
        ' -------------------------------------------------
        ' Create and show a specific UI depending on the
        ' currently selected data
        ' -------------------------------------------------
        'Dim SelectedData As ApsimFile.Component = Controller.ApsimData.Find(Controller.SelectedPath)
        If Not IsNothing(SelectedComponent) Then
            If CurrentUIIndex = -1 OrElse UITypes(CurrentUIIndex) <> SelectedComponent.Type Then
                CloseUI()
                CurrentUIIndex = UITypes.IndexOf(SelectedComponent.Type)
                If CurrentUIIndex = -1 Then
                    Dim View As BaseView = Controller.CreateUI(SelectedComponent.Type)
                    If Not IsNothing(View) Then
                        UIs.Add(View)
                        UITypes.Add(SelectedComponent.Type)
                        CurrentUIIndex = UIs.Count - 1
                    End If
                End If
            Else
                SaveCurrentView()
            End If
            If CurrentUIIndex <> -1 Then
                Try
                    Dim View As BaseView = UIs(CurrentUIIndex)
                    View.OnLoad(Controller, SelectedPath, SelectedComponent.Contents)
                    View.Parent = UIPanel
                    View.Dock = DockStyle.Fill
                    View.Show()
                    View.OnRefresh()
                Catch ex As Exception
                    MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                End Try
            End If
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
            'if path has a delimiter at the beginning it is normal view
            'else it is a factorial view
            Dim Comp As ApsimFile.Component '
            Dim pos As Integer = View.NodePath.IndexOf(ApsimFile.Component.Delimiter)
            If (pos = 0) Then
                Comp = Controller.ApsimData.Find(View.NodePath)
            Else
                Comp = Controller.FindFactorialComponent(View.NodePath)
            End If
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

        'Parent panel is either the main form, or the Component tab within Factorial
        'if Component tab not active then don't display the UI
        If Not Controller.FactorialMode Then
            UIPanel.Parent = pnlDisplayArea

            'If there is only 1 node that is selected
            If Controller.SelectedPaths.Count = 1 Then
                ShowUI(Controller.SelectedPath, Controller.Selection)        'show the corresponding UI for that node in the panel to the right of the tree
            Else
                CloseUI()       'show no UI in the panel to the right of the tree
            End If
        End If

        Windows.Forms.Cursor.Current = SavedCursor                  'restore the cursor object to what it was before the wait cursor.
    End Sub
    Private Sub OnFactorialSelectionChanged(ByVal OldSelection As String, ByVal NewSelection As String)
        ' -----------------------------------------------------
        ' User has selected a node - update user interface
        ' -----------------------------------------------------
        Visible = True                                              'make the ExplorerUI visible
        Dim SavedCursor As Cursor = Windows.Forms.Cursor.Current    'store the current cursor object (usually an arrow)
        Windows.Forms.Cursor.Current = Cursors.WaitCursor           'set the cursor object to the default cursor object used for waiting (usually an hourglass) 

        UIPanel.Parent = Panel1

        'If there is only 1 node that is selected
        If NewSelection <> "" Then
            ShowUI(Controller.SelectedFactorialPath, Controller.FactorialSelection)        'show the corresponding UI for that node in the panel to the right of the tree
        Else
            CloseUI()       'show no UI in the panel to the right of the tree
        End If

        Windows.Forms.Cursor.Current = SavedCursor                  'restore the cursor object to what it was before the wait cursor.
    End Sub
    Public Sub RefreshDisplayMode()
        OnBeforeSave()

        Visible = True                                              'make the ExplorerUI visible
        Dim SavedCursor As Cursor = Windows.Forms.Cursor.Current    'store the current cursor object (usually an arrow)
        Windows.Forms.Cursor.Current = Cursors.WaitCursor           'set the cursor object to the default cursor object used for waiting (usually an hourglass) 

        If Controller.FactorialMode Then
            pnlHost.Visible = True
            UIPanel.Parent = Panel1
            If Controller.SelectedFactorialPath = "" Then
                Controller.SelectedFactorialPath = Controller.ApsimData.FactorComponent.FullPath
            Else
                OnFactorialSelectionChanged(Controller.SelectedFactorialPath, Controller.SelectedFactorialPath)
            End If
        Else
            UIPanel.Parent = pnlDisplayArea
            pnlHost.Visible = False
            OnSelectionChanged(Controller.SelectedPaths, Controller.SelectedPaths)
        End If
        Windows.Forms.Cursor.Current = SavedCursor                  'restore the cursor object to what it was before the wait cursor.
    End Sub

    Private Sub FactorTabControl_Selecting(ByVal sender As System.Object, ByVal e As System.Windows.Forms.TabControlCancelEventArgs)
    End Sub
End Class
