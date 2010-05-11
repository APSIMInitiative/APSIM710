
Imports System
Imports System.Collections
Imports System.Collections.Specialized
Imports System.io
Imports System.IO.Path
Imports System.Reflection
Imports System.xml
Imports System.xml.XmlNodeList

Imports ApsimFile
Imports Controllers
Imports CSGeneral
Imports UIBits          'OptionsForm
Imports GraphDataUserInterface


Public Class MainUI
    Inherits System.Windows.Forms.Form

    Private Args As New StringCollection
    Private ApplicationName As String
    Private SimFileName As String


    Private SimulationController As BaseController                          'BaseController for Simulation
    Friend WithEvents SimulationExplorer As Controllers.ExplorerUI          'ExplorerUI for Simulation
    Friend WithEvents SimulationContainer As System.Windows.Forms.ToolStripContainer
    Friend WithEvents SimulationToolStrip As System.Windows.Forms.ToolStrip


    Private ToolboxController As BaseController                             'BaseController for Toolbox
    Private ToolboxExplorer As ExplorerUI                                   'ExplorerUI for Toolbox
    Private ToolBoxSplitterPoint As Integer
    Friend WithEvents ToolboxSplitter As System.Windows.Forms.Splitter
    Friend WithEvents ToolBoxesToolStrip As System.Windows.Forms.ToolStrip
    Friend WithEvents ToolBoxPanel As System.Windows.Forms.Panel
    Friend WithEvents ToolBoxPanelToolBar As System.Windows.Forms.ToolBar
    Friend WithEvents ToolBoxToolBarPanel As System.Windows.Forms.Panel
    Friend WithEvents Label5 As System.Windows.Forms.Label                  'label on the Toolbox that says "Toolbox" 
    Friend WithEvents Label6 As System.Windows.Forms.Label                  'same on only the forecolour is Highlight and not HotTrack
    Friend WithEvents ToolboxButtonClose As System.Windows.Forms.Button

    Friend WithEvents ContextMenuStrip1 As System.Windows.Forms.ContextMenuStrip        'I think that this is no longer used. I think I can delete this
    Friend WithEvents ToolStripMenuItem1 As System.Windows.Forms.ToolStripMenuItem      'I think that this is no longer used. I think I can delete this


    Private ApsimProcess As ProcessCaller
    Private CurrentRunningSimulationIndex As Integer
    Private CurrentStartDate As Date
    Private CurrentEndDate As Date
    Private CurrentSummaryFile As StreamWriter = Nothing
    Friend WithEvents RunToolStrip As System.Windows.Forms.ToolStrip
    Friend WithEvents RunProgress As System.Windows.Forms.ToolStripProgressBar
    Friend WithEvents PauseButton As System.Windows.Forms.ToolStripButton
    Friend WithEvents StopButton As System.Windows.Forms.ToolStripButton
    Friend WithEvents ErrorsButton As System.Windows.Forms.ToolStripButton
    Friend WithEvents RunButton As System.Windows.Forms.ToolStripButton
    Friend WithEvents PercentLabel As System.Windows.Forms.ToolStripLabel
    Private CurrentErrors As New StringCollection



#Region "Constructor / Destructor / Main"
    <System.STAThread()> _
    Public Shared Sub Main(ByVal Args() As String)
        Application.EnableVisualStyles()
        Application.DoEvents()
        Application.DoEvents()
        Application.Run(New MainUI(Args))
        Application.DoEvents()
        Application.DoEvents()
    End Sub
    Public Sub New(ByVal cmdArgs() As String)
        MyBase.New()

      Try

         'This call is required by the Windows Form Designer.
         InitializeComponent()

         ' Get application name.
         ApplicationName = ""
         If Not IsNothing(cmdArgs) Then
            For Each Arg As String In cmdArgs
               If (ApplicationName = "" And Arg(0) = "/") Then
                  ApplicationName = Arg.Substring(1)
               Else
                  Args.Add(Arg)
               End If
            Next
         End If
         If ApplicationName = "" Then
            ApplicationName = "ApsimUI"
         End If

         RunToolStrip.Visible = ApplicationName = "ApsimUI"

            ' Create our controller
            Configuration.Instance.ApplicationName = ApplicationName
            PlugIns.LoadAll()
            SimulationController = New BaseController(Me, ApplicationName, True)

         ' Display splash screen
         If Configuration.Instance.Setting("SplashScreen") <> "" And Args.Count = 0 Then
            Dim SplashForm As Form = BaseController.CreateClass(Configuration.Instance.Setting("SplashScreen"))
            If Configuration.Instance.Setting("SplashScreenButtonVisible").ToLower = "yes" Then
               SplashForm.ShowDialog()
            Else
               SplashForm.Show()
               Application.DoEvents()
            End If
         End If
      Catch ex As Exception
         MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
      End Try

      ' Position window correctly.
      Try
         Dim inifile As New IniFile
         WindowState = Convert.ToInt32(Configuration.Instance.Setting("windowstate"))
         Top = Convert.ToInt32(Configuration.Instance.Setting("top"))
         Left = Convert.ToInt32(Configuration.Instance.Setting("left"))
         If (Left < 0 Or Left > Width) Then
            Left = 1
         End If
         If (Top < 0 Or Top > Height) Then
            Top = 1
         End If
         Height = Convert.ToInt32(Configuration.Instance.Setting("height"))
         Width = Convert.ToInt32(Configuration.Instance.Setting("width"))
         If (Height = 0 Or Width = 0) Then
            Height = 600
            Width = 400
         End If
      Catch ex As System.Exception
         Me.WindowState = FormWindowState.Normal
      End Try

    End Sub
    Protected Overloads Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing Then
            If Not (components Is Nothing) Then
                components.Dispose()
            End If
        End If
        MyBase.Dispose(disposing)
    End Sub
    Public Sub Go(ByVal CommandLine As String)
        Args = StringManip.SplitStringHonouringQuotes(CommandLine, " ")
        Me.Show()
        Application.Run(Me)
    End Sub
#End Region

#Region "Windows Form Designer generated code "


    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.
    'Do not modify it using the code editor.

    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(MainUI))
        Me.ToolBoxPanel = New System.Windows.Forms.Panel
        Me.ToolBoxToolBarPanel = New System.Windows.Forms.Panel
        Me.Label6 = New System.Windows.Forms.Label
        Me.Label5 = New System.Windows.Forms.Label
        Me.ToolboxButtonClose = New System.Windows.Forms.Button
        Me.ToolBoxPanelToolBar = New System.Windows.Forms.ToolBar
        Me.SimulationContainer = New System.Windows.Forms.ToolStripContainer
        Me.ToolBoxesToolStrip = New System.Windows.Forms.ToolStrip
        Me.SimulationExplorer = New Controllers.ExplorerUI
        Me.ToolboxSplitter = New System.Windows.Forms.Splitter
        Me.SimulationToolStrip = New System.Windows.Forms.ToolStrip
        Me.RunToolStrip = New System.Windows.Forms.ToolStrip
        Me.RunButton = New System.Windows.Forms.ToolStripButton
        Me.PauseButton = New System.Windows.Forms.ToolStripButton
        Me.StopButton = New System.Windows.Forms.ToolStripButton
        Me.RunProgress = New System.Windows.Forms.ToolStripProgressBar
        Me.PercentLabel = New System.Windows.Forms.ToolStripLabel
        Me.ErrorsButton = New System.Windows.Forms.ToolStripButton
        Me.ContextMenuStrip1 = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.ToolStripMenuItem1 = New System.Windows.Forms.ToolStripMenuItem
        Me.ToolBoxPanel.SuspendLayout()
        Me.ToolBoxToolBarPanel.SuspendLayout()
        Me.SimulationContainer.BottomToolStripPanel.SuspendLayout()
        Me.SimulationContainer.ContentPanel.SuspendLayout()
        Me.SimulationContainer.TopToolStripPanel.SuspendLayout()
        Me.SimulationContainer.SuspendLayout()
        Me.RunToolStrip.SuspendLayout()
        Me.ContextMenuStrip1.SuspendLayout()
        Me.SuspendLayout()
        '
        'ToolBoxPanel
        '
        Me.ToolBoxPanel.Controls.Add(Me.ToolBoxToolBarPanel)
        Me.ToolBoxPanel.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.ToolBoxPanel.Location = New System.Drawing.Point(0, 410)
        Me.ToolBoxPanel.Name = "ToolBoxPanel"
        Me.ToolBoxPanel.Size = New System.Drawing.Size(735, 104)
        Me.ToolBoxPanel.TabIndex = 12
        Me.ToolBoxPanel.Visible = False
        '
        'ToolBoxToolBarPanel
        '
        Me.ToolBoxToolBarPanel.BackColor = System.Drawing.SystemColors.Highlight
        Me.ToolBoxToolBarPanel.Controls.Add(Me.Label6)
        Me.ToolBoxToolBarPanel.Controls.Add(Me.Label5)
        Me.ToolBoxToolBarPanel.Controls.Add(Me.ToolboxButtonClose)
        Me.ToolBoxToolBarPanel.Controls.Add(Me.ToolBoxPanelToolBar)
        Me.ToolBoxToolBarPanel.Dock = System.Windows.Forms.DockStyle.Top
        Me.ToolBoxToolBarPanel.Location = New System.Drawing.Point(0, 0)
        Me.ToolBoxToolBarPanel.Name = "ToolBoxToolBarPanel"
        Me.ToolBoxToolBarPanel.Size = New System.Drawing.Size(735, 24)
        Me.ToolBoxToolBarPanel.TabIndex = 19
        '
        'Label6
        '
        Me.Label6.AutoSize = True
        Me.Label6.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label6.ForeColor = System.Drawing.SystemColors.HighlightText
        Me.Label6.Location = New System.Drawing.Point(3, 3)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(58, 16)
        Me.Label6.TabIndex = 22
        Me.Label6.Text = "Toolbox"
        '
        'Label5
        '
        Me.Label5.AutoSize = True
        Me.Label5.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label5.ForeColor = System.Drawing.SystemColors.HotTrack
        Me.Label5.Location = New System.Drawing.Point(3, 7)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(58, 16)
        Me.Label5.TabIndex = 22
        Me.Label5.Text = "Toolbox"
        '
        'ToolboxButtonClose
        '
        Me.ToolboxButtonClose.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.ToolboxButtonClose.BackColor = System.Drawing.Color.Transparent
        Me.ToolboxButtonClose.BackgroundImage = CType(resources.GetObject("ToolboxButtonClose.BackgroundImage"), System.Drawing.Image)
        Me.ToolboxButtonClose.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.ToolboxButtonClose.Location = New System.Drawing.Point(706, -1)
        Me.ToolboxButtonClose.Name = "ToolboxButtonClose"
        Me.ToolboxButtonClose.Size = New System.Drawing.Size(24, 24)
        Me.ToolboxButtonClose.TabIndex = 20
        Me.ToolboxButtonClose.TabStop = False
        Me.ToolboxButtonClose.UseVisualStyleBackColor = False
        '
        'ToolBoxPanelToolBar
        '
        Me.ToolBoxPanelToolBar.Appearance = System.Windows.Forms.ToolBarAppearance.Flat
        Me.ToolBoxPanelToolBar.Divider = False
        Me.ToolBoxPanelToolBar.Dock = System.Windows.Forms.DockStyle.Fill
        Me.ToolBoxPanelToolBar.DropDownArrows = True
        Me.ToolBoxPanelToolBar.Location = New System.Drawing.Point(0, 0)
        Me.ToolBoxPanelToolBar.Name = "ToolBoxPanelToolBar"
        Me.ToolBoxPanelToolBar.ShowToolTips = True
        Me.ToolBoxPanelToolBar.Size = New System.Drawing.Size(735, 26)
        Me.ToolBoxPanelToolBar.TabIndex = 17
        Me.ToolBoxPanelToolBar.TextAlign = System.Windows.Forms.ToolBarTextAlign.Right
        '
        'SimulationContainer
        '
        '
        'SimulationContainer.BottomToolStripPanel
        '
        Me.SimulationContainer.BottomToolStripPanel.BackColor = System.Drawing.SystemColors.ControlLight
        Me.SimulationContainer.BottomToolStripPanel.Controls.Add(Me.ToolBoxesToolStrip)
        '
        'SimulationContainer.ContentPanel
        '
        Me.SimulationContainer.ContentPanel.AutoScroll = True
        Me.SimulationContainer.ContentPanel.BackColor = System.Drawing.SystemColors.Window
        Me.SimulationContainer.ContentPanel.Controls.Add(Me.SimulationExplorer)
        Me.SimulationContainer.ContentPanel.Controls.Add(Me.ToolboxSplitter)
        Me.SimulationContainer.ContentPanel.Controls.Add(Me.ToolBoxPanel)
        Me.SimulationContainer.ContentPanel.Size = New System.Drawing.Size(735, 514)
        Me.SimulationContainer.Dock = System.Windows.Forms.DockStyle.Fill
        Me.SimulationContainer.Location = New System.Drawing.Point(0, 0)
        Me.SimulationContainer.Name = "SimulationContainer"
        Me.SimulationContainer.Size = New System.Drawing.Size(735, 583)
        Me.SimulationContainer.TabIndex = 4
        Me.SimulationContainer.Text = "ToolStripContainer1"
        '
        'SimulationContainer.TopToolStripPanel
        '
        Me.SimulationContainer.TopToolStripPanel.BackColor = System.Drawing.SystemColors.ControlLight
        Me.SimulationContainer.TopToolStripPanel.Controls.Add(Me.SimulationToolStrip)
        Me.SimulationContainer.TopToolStripPanel.Controls.Add(Me.RunToolStrip)
        '
        'ToolBoxesToolStrip
        '
        Me.ToolBoxesToolStrip.Dock = System.Windows.Forms.DockStyle.None
        Me.ToolBoxesToolStrip.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden
        Me.ToolBoxesToolStrip.LayoutStyle = System.Windows.Forms.ToolStripLayoutStyle.HorizontalStackWithOverflow
        Me.ToolBoxesToolStrip.Location = New System.Drawing.Point(3, 0)
        Me.ToolBoxesToolStrip.Name = "ToolBoxesToolStrip"
        Me.ToolBoxesToolStrip.ShowItemToolTips = False
        Me.ToolBoxesToolStrip.Size = New System.Drawing.Size(102, 25)
        Me.ToolBoxesToolStrip.TabIndex = 2
        '
        'SimulationExplorer
        '
        Me.SimulationExplorer.Dock = System.Windows.Forms.DockStyle.Fill
        Me.SimulationExplorer.Location = New System.Drawing.Point(0, 0)
        Me.SimulationExplorer.Name = "SimulationExplorer"
        Me.SimulationExplorer.Size = New System.Drawing.Size(735, 407)
        Me.SimulationExplorer.TabIndex = 36
        '
        'ToolboxSplitter
        '
        Me.ToolboxSplitter.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.ToolboxSplitter.Location = New System.Drawing.Point(0, 407)
        Me.ToolboxSplitter.Name = "ToolboxSplitter"
        Me.ToolboxSplitter.Size = New System.Drawing.Size(735, 3)
        Me.ToolboxSplitter.TabIndex = 25
        Me.ToolboxSplitter.TabStop = False
        Me.ToolboxSplitter.Visible = False
        '
        'SimulationToolStrip
        '
        Me.SimulationToolStrip.Dock = System.Windows.Forms.DockStyle.None
        Me.SimulationToolStrip.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden
        Me.SimulationToolStrip.Location = New System.Drawing.Point(3, 0)
        Me.SimulationToolStrip.Name = "SimulationToolStrip"
        Me.SimulationToolStrip.Size = New System.Drawing.Size(102, 25)
        Me.SimulationToolStrip.TabIndex = 1
        '
        'RunToolStrip
        '
        Me.RunToolStrip.Dock = System.Windows.Forms.DockStyle.None
        Me.RunToolStrip.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.RunButton, Me.PauseButton, Me.StopButton, Me.RunProgress, Me.PercentLabel, Me.ErrorsButton})
        Me.RunToolStrip.Location = New System.Drawing.Point(105, 0)
        Me.RunToolStrip.Name = "RunToolStrip"
        Me.RunToolStrip.Size = New System.Drawing.Size(217, 44)
        Me.RunToolStrip.TabIndex = 2
        '
        'RunButton
        '
        Me.RunButton.Image = CType(resources.GetObject("RunButton.Image"), System.Drawing.Image)
        Me.RunButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None
        Me.RunButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.RunButton.Name = "RunButton"
        Me.RunButton.Size = New System.Drawing.Size(30, 41)
        Me.RunButton.Text = "Run"
        Me.RunButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText
        Me.RunButton.ToolTipText = "Run APSIM"
        '
        'PauseButton
        '
        Me.PauseButton.CheckOnClick = True
        Me.PauseButton.Enabled = False
        Me.PauseButton.Image = CType(resources.GetObject("PauseButton.Image"), System.Drawing.Image)
        Me.PauseButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None
        Me.PauseButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.PauseButton.Name = "PauseButton"
        Me.PauseButton.Size = New System.Drawing.Size(40, 41)
        Me.PauseButton.Text = "Pause"
        Me.PauseButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText
        Me.PauseButton.ToolTipText = "Pause APSIM"
        '
        'StopButton
        '
        Me.StopButton.Enabled = False
        Me.StopButton.Image = CType(resources.GetObject("StopButton.Image"), System.Drawing.Image)
        Me.StopButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None
        Me.StopButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.StopButton.Name = "StopButton"
        Me.StopButton.Size = New System.Drawing.Size(33, 41)
        Me.StopButton.Text = "Stop"
        Me.StopButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText
        Me.StopButton.ToolTipText = "Stop APSIM"
        '
        'RunProgress
        '
        Me.RunProgress.AutoSize = False
        Me.RunProgress.Name = "RunProgress"
        Me.RunProgress.Size = New System.Drawing.Size(100, 24)
        Me.RunProgress.Step = 1
        Me.RunProgress.Style = System.Windows.Forms.ProgressBarStyle.Continuous
        '
        'PercentLabel
        '
        Me.PercentLabel.Name = "PercentLabel"
        Me.PercentLabel.Size = New System.Drawing.Size(0, 41)
        '
        'ErrorsButton
        '
        Me.ErrorsButton.Image = CType(resources.GetObject("ErrorsButton.Image"), System.Drawing.Image)
        Me.ErrorsButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None
        Me.ErrorsButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ErrorsButton.Name = "ErrorsButton"
        Me.ErrorsButton.Size = New System.Drawing.Size(71, 41)
        Me.ErrorsButton.Text = "Errors found"
        Me.ErrorsButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText
        Me.ErrorsButton.Visible = False
        '
        'ContextMenuStrip1
        '
        Me.ContextMenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripMenuItem1})
        Me.ContextMenuStrip1.Name = "ContextMenuStrip1"
        Me.ContextMenuStrip1.Size = New System.Drawing.Size(182, 26)
        '
        'ToolStripMenuItem1
        '
        Me.ToolStripMenuItem1.Name = "ToolStripMenuItem1"
        Me.ToolStripMenuItem1.Size = New System.Drawing.Size(181, 22)
        Me.ToolStripMenuItem1.Text = "ToolStripMenuItem1"
        '
        'MainUI
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(735, 583)
        Me.Controls.Add(Me.SimulationContainer)
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.KeyPreview = True
        Me.Name = "MainUI"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.Manual
        Me.Text = "APSIM"
        Me.ToolBoxPanel.ResumeLayout(False)
        Me.ToolBoxToolBarPanel.ResumeLayout(False)
        Me.ToolBoxToolBarPanel.PerformLayout()
        Me.SimulationContainer.BottomToolStripPanel.ResumeLayout(False)
        Me.SimulationContainer.BottomToolStripPanel.PerformLayout()
        Me.SimulationContainer.ContentPanel.ResumeLayout(False)
        Me.SimulationContainer.TopToolStripPanel.ResumeLayout(False)
        Me.SimulationContainer.TopToolStripPanel.PerformLayout()
        Me.SimulationContainer.ResumeLayout(False)
        Me.SimulationContainer.PerformLayout()
        Me.RunToolStrip.ResumeLayout(False)
        Me.RunToolStrip.PerformLayout()
        Me.ContextMenuStrip1.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

#End Region

    Private Sub OnMainFormLoad(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Load
      Try

         AddHandler SimulationController.ApsimData.DirtyChanged, AddressOf OnDirtyChanged
         AddHandler SimulationController.ApsimData.FileNameChanged, AddressOf OnFileNameChanged

         ' Load some assemblies for later. The code for some actions are found in
         ' these assemblies.
         Assembly.Load("Actions")
         Assembly.Load("CSUserInterface")
         Assembly.Load("VBUserInterface")
         Assembly.Load("GraphDataUserInterface")
         'Assembly.Load("Soils")

         'Try and load an icon from configuration. (Splash Screen?)
         Dim IconFileName As String = Configuration.Instance.Setting("Icon")
         If IconFileName <> "" AndAlso File.Exists(IconFileName) Then
            Icon = New System.Drawing.Icon(IconFileName)
         End If

         'Create the MainToolBar
         SimulationController.ProvideToolStrip(SimulationToolStrip, "MainToolBar")
         If Configuration.Instance.Setting("HideMainMenu") = "Yes" Then
            SimulationToolStrip.Visible = False
         End If

         'Show the Simulation Explorer.
         SimulationExplorer.OnLoad(SimulationController)
         SimulationController.Explorer = SimulationExplorer    'give the explorer ui to the controller.

         ' Process command line arguments.
         ' Load a file if one was specified on the command line.
            Dim ExportDirectory As String = ""
            Dim ExportExtension As String = ""
         If Control.ModifierKeys <> Keys.Control And Args.Count > 0 Then
            For Each Arg As String In Args
                    If Arg = "Export" And Args.Count = 4 Then
                        ExportDirectory = Args(2)
                        ExportExtension = Args(3)
                        Exit For
                    Else
                        Dim FileName As String = Arg.Replace("""", "")
                        If FileName.Length() > 0 Then
                            If Path.GetFileName(FileName).ToLower() = "response.file" Then
                                Dim Wizard As New GraphWizardForm
                                Wizard.Go(SimulationController, FileName)
                                Wizard.ShowDialog()
                            Else
                                SimulationController.ApsimData.OpenFile(FileName)
                            End If
                        End If
                    End If

            Next
         End If

         ' If no file loaded then load previous one.
         If Control.ModifierKeys <> Keys.Control And SimulationController.ApsimData.FileName = Nothing Then
            SimulationController.LoadPreviousFile()
         End If

         ' If we have an export file name then do an export.
            If ExportDirectory <> "" Then
                Actions.BaseActions.ExportAll(SimulationController, SimulationController.ApsimData.RootComponent, ExportDirectory, ExportExtension)
                Close()
            Else
                'Create the Toolbox Explorer
                Dim ToolboxesVisible As Boolean = Configuration.Instance.Setting("ToolboxesVisible").ToLower = "yes"
                If ToolboxesVisible Then
                    ' Setup but don't show the Toolbox Explorer.
                    ToolboxController = New BaseController(Nothing, ApplicationName, False)

                    ToolboxExplorer = New ExplorerUI()
                    ToolboxExplorer.Name = "ToolboxExplorer"
                    ToolboxExplorer.Parent = ToolBoxPanel
                    ToolboxExplorer.Dock = DockStyle.Fill
                    ToolboxExplorer.BringToFront()
                    ToolboxExplorer.OnLoad(ToolboxController)

                    ToolboxController.Explorer = ToolboxExplorer    'give the toolbox ExplorerUI to the toolbox controller.
                    Try
                        PopulateToolBoxStrip()                          'populate the Toolbox Strip with all the different Toolboxes
                    Catch ex As Exception
                        MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                    End Try

                Else
                    ToolBoxesToolStrip.Visible = False
                End If
            End If

      Catch ex As Exception
         MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
      End Try

    End Sub
    Private Sub OnMainFormClosing(ByVal sender As Object, ByVal e As System.ComponentModel.CancelEventArgs) Handles MyBase.Closing
        ' User is closing down - save our work.
        e.Cancel = Not SimulationController.FileSaveAfterPrompt()
        If Not e.Cancel Then
            'on closing save the current window state (normal, minimised, maximized) and save the position and height and width of window, 
            'to apsim.xml (see between <ApsimUI> tags)
            Try
                If Not (Me.WindowState = 1) Then    'don't save the state if the window was minimised when it was closed. 
                    Configuration.Instance.SetSetting("windowstate", Str(Me.WindowState))
                End If
                If (Me.Top >= 0) And (Me.Left >= 0) And (Me.Width > 0) And (Me.Height > 0) Then   'must be sensible values (non negative)    
                    Configuration.Instance.SetSetting("top", Str(Me.Top))
                    Configuration.Instance.SetSetting("left", Str(Me.Left))
                    Configuration.Instance.SetSetting("width", Str(Me.Width))
                    Configuration.Instance.SetSetting("height", Str(Me.Height))
                End If
            Catch ex As System.Exception
            End Try

            If Not IsNothing(ToolboxExplorer) AndAlso ToolboxExplorer.Visible AndAlso Not ToolboxController.ApsimData.IsReadOnly Then
                ToolboxController.ApsimData.Save()
            End If
            ApsimRunToolStrip.Instance.OnStop()
        End If
    End Sub
    Private Sub OnDirtyChanged(ByVal IsDirty As Boolean)
        UpdateCaption()
    End Sub
    Private Sub OnFileNameChanged(ByVal FileName As String)
        UpdateCaption()
    End Sub
    Private Sub UpdateCaption()
        ' ----------------------------------------
        ' Called to update the main form's caption
        ' ----------------------------------------
        If SimulationController.ApsimData.IsReadOnly Then
            Text = ApplicationName + " - " + SimulationController.ApsimData.FileName + " [readonly]"
        ElseIf SimulationController.ApsimData.IsDirty Then
            Text = ApplicationName + " - " + SimulationController.ApsimData.FileName + " * "
        Else
            Text = ApplicationName + " - " + SimulationController.ApsimData.FileName
        End If
    End Sub
#Region "Toolbox button bar"


    Public Shared Sub Options(ByVal Controller As BaseController)
        ' ---------------------------------------------------------------
        ' User wants to modify user interface options.
        ' ---------------------------------------------------------------
        Dim Form As New OptionsForm
        Form.ShowDialog()
        Dim F As MainUI = Controller.MainForm
        PlugIns.LoadAll()
        F.PopulateToolBoxStrip()
        F.SimulationToolStrip.Visible = Configuration.Instance.Setting("HideMainMenu") <> "Yes"
    End Sub

    Public Sub PopulateToolBoxStrip()
        ' ---------------------------------------------------------------
        ' Populate the toolbox strip with buttons for each toolbox.
        ' ---------------------------------------------------------------

        'Remove existing buttons first.
        ToolBoxesToolStrip.Items.Clear()

        ' Loop through each of the known toolboxes
        For Each FileName As String In Toolboxes.Instance.AllToolBoxes
            If File.Exists(FileName) Then
                Dim Doc As New XmlDocument
                Doc.Load(FileName)

                ' Get the image attribute from the root node of the loaded xml file
                Dim ImageFileName As String = XmlHelper.Attribute(Doc.DocumentElement, "image")
                If ImageFileName = "" Then
                    ImageFileName = "%apsim%\UserInterface\Images\Toolbox24.png"
                End If
                ImageFileName = Configuration.RemoveMacros(ImageFileName)

                Dim ToolBoxName As String = Path.GetFileNameWithoutExtension(FileName)
                Dim NewItem As New ToolStripButton(ToolBoxName, New System.Drawing.Bitmap(ImageFileName))
                NewItem.TextImageRelation = TextImageRelation.ImageBeforeText
                NewItem.ImageScaling = ToolStripItemImageScaling.None
                NewItem.CheckOnClick = True
                NewItem.ToolTipText = ""
                NewItem.Tag = FileName
                AddHandler NewItem.Click, AddressOf OnToolBoxClick
                ToolBoxesToolStrip.Items.Add(NewItem)
            Else
                MessageBox.Show("Cannot find toolbox file: " + FileName, _
                                "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End If

        Next
    End Sub
    Private Sub OnToolBoxClick(ByVal Sender As Object, ByVal e As System.EventArgs)
        ' ---------------------------------------------------------------
        ' Display the given ToolBoxName in the toolbox panel at
        ' ---------------------------------------------------------------
        ToolboxController.ApsimData.Save()    'Save any changes made to the Toolbox.

        Dim ButtonThatWasClicked As ToolStripButton = Sender
        If Not ButtonThatWasClicked.Checked Then
            HideToolBoxWindow(ButtonThatWasClicked, e)
        Else
            ' Turn off the checked status of all toolbox buttons - except the one
            ' that was just clicked.
            For i As Integer = 2 To ToolBoxesToolStrip.Items.Count - 1
                Dim Button As ToolStripButton = ToolBoxesToolStrip.Items(i)
                If Not Button Is ButtonThatWasClicked Then
                    Button.Checked = False
                End If
            Next

            Dim inifile As New IniFile
            ToolBoxPanel.Height = Val(Configuration.Instance.Setting("toolboxheight"))
            ToolBoxPanel.Height = ToolBoxPanel.Height - 1
            ToolBoxPanel.Height = ToolBoxPanel.Height + 1

            ToolboxSplitter.Visible = True
            ToolBoxPanel.Visible = True
            Me.ToolBoxSplitterPoint = ToolboxSplitter.SplitPosition

            Dim ToolBoxButton As ToolStripButton = Sender
            Dim filename As String = ToolBoxButton.Tag
            ToolboxController.ApsimData.OpenFile(filename)

        End If
    End Sub
    Private Sub HideToolBoxWindow(ByVal Sender As Object, ByVal e As EventArgs) Handles ToolboxButtonClose.Click
        ' ---------------------------------------------------------------
        ' Hide the toolbox window.
        ' ---------------------------------------------------------------

        'This is what closes the toolbox when you hit the close button. It does not actually close anything it just makes the Toolbox ExplorerUI invisible

        ' Turn off the checked status of all toolbox buttons.           'a particular toolbox button is checked on the toolbox strip when it is open in ToolboxExplorer 
        For i As Integer = 2 To ToolBoxesToolStrip.Items.Count - 1
            Dim Button As ToolStripButton = ToolBoxesToolStrip.Items(i)
            Button.Checked = False
        Next

        ToolboxController.ApsimData.Save()                                'Save any changes made to the Toolbox.

        ToolBoxPanel.Visible = False                            'This is what makes the toolbox disappear. There is no actual close.
        ToolboxSplitter.Visible = ToolBoxPanel.Visible
    End Sub
    Private Sub ToolBoxSplitter_LocationChanged(ByVal sender As Object, ByVal e As SplitterEventArgs) Handles ToolboxSplitter.SplitterMoved
        ' ---------------------------------------------------------------
        ' Whenever the user moves the toolbox splitter, save the position
        ' ---------------------------------------------------------------
        If ToolBoxPanel.Visible Then
            Configuration.Instance.SetSetting("toolboxheight", Str(ToolBoxPanel.Height))
        End If
    End Sub

#End Region


    Private Sub OnRunButtonClick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RunButton.Click
        SimulationController.InvokeAction(Nothing, "Run")
    End Sub

    Private Sub OnPauseClick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PauseButton.Click
        ApsimRunToolStrip.Instance.OnPause()
    End Sub

    Private Sub OnErrorsClick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ErrorsButton.Click
        Dim ErrorNodePath As String = ApsimRunToolStrip.Instance.GetSimulationWithError()
        If ErrorNodePath <> "" Then
            ErrorNodePath = ErrorNodePath + "/SummaryFile"
            If Not IsNothing(SimulationController.ApsimData.Find(ErrorNodePath)) Then
                SimulationController.SelectedPath = ErrorNodePath
            End If

        End If
    End Sub

    Private Sub OnStopClick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles StopButton.Click
        ApsimRunToolStrip.Instance.OnStop()
    End Sub

End Class
