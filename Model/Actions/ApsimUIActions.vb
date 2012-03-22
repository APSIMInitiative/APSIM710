
Imports System.IO
Imports System.Collections.Specialized
Imports System.Collections.Generic
Imports System.Windows.Forms

Imports System.Xml

Imports ApsimFile
Imports Controllers
Imports CSGeneral
Imports UIUtility




Public Class ApsimUIActions
   Public Shared Sub FileNew(ByVal Controller As BaseController)
      If Controller.FileSaveAfterPrompt() Then
         Dim dialog As New System.Windows.Forms.OpenFileDialog
         Dim NewSimFolder As String = Configuration.Instance.Setting("NewSimulationFolder")
         dialog.InitialDirectory = NewSimFolder
         dialog.Title = "New Simulation"
         dialog.Filter = Configuration.Instance.Setting("DialogFilter")          'only show .apsim files (this changes to .soils file if APSoil not ApsimUI). 
         dialog.DefaultExt = Configuration.Instance.Setting("DefaultExtension")  'once again changes to .soils when APSoil
         dialog.Multiselect = False                      'don't let them select multiple files
         If dialog.ShowDialog = System.Windows.Forms.DialogResult.OK Then
            Controller.Explorer.CloseUI()                           'close whatever simulation is currently in the ExplorerUI
            Controller.ApsimData.NewFromFile(dialog.FileName)          'store the xml in the .apsim file into the ApsimData variable in the Controller
         End If
      End If
   End Sub

    Public Shared Sub HelpDocumentation(ByVal Controller As BaseController)
        Dim HelpURL As String = Configuration.Instance.Setting("docfile")
        Process.Start(HelpURL)
    End Sub
    Public Shared Sub ClusterHelpDocumentation(ByVal Controller As BaseController)
        Dim HelpURL As String = Configuration.Instance.Setting("ClusterHelpPage")
        Process.Start(HelpURL)
    End Sub
    Public Shared Sub ApsimSearchEngine(ByVal Controller As BaseController)
        Dim url As String = Configuration.Instance.Setting("ApsimSearchEngine")
        Process.Start(url)
    End Sub
   Public Shared Sub ApsimInternetGroup(ByVal Controller As BaseController)
      Dim url As String = Configuration.Instance.Setting("ApsimInternetGroup")
      Process.Start(url)
   End Sub



#Region "Simulation methods"
   Public Shared Sub Run(ByVal Controller As BaseController)
      ' ------------------------------------------------
      ' Go looking for simulations to run. Look at the
      ' currently selected nodes first and progressively
      ' their parents until some simulations are found.
      ' ------------------------------------------------
      If Configuration.Instance.Setting("ReloadPlugInsBeforeRunningAPSIM") = "Yes" Then
         PlugIns.LoadAll()
      End If

        If (BaseActions.FileSave(Controller)) Then
            Dim RunPanels As Control() = Controller.MainForm.Controls.Find("RunToolStrip", True)
            If RunPanels.Length = 1 Then
                ApsimRunToolStrip.Instance.RunApsim(RunPanels(0), Controller) '_
                'Controller.ApsimData, _
                'Controller.SelectedPaths)
            End If
        End If
    End Sub
   Public Shared Sub CreateSIM(ByVal Controller As BaseController)
      ' ------------------------------------------------
      ' Create a .sim file.
      ' ------------------------------------------------
        If (BaseActions.FileSave(Controller)) Then
            Dim RunPanels As Control() = Controller.MainForm.Controls.Find("RunToolStrip", True)
            If RunPanels.Length = 1 Then
                ApsimRunToolStrip.Instance.CreateSIM(RunPanels(0), Controller)
            End If
        End If
    End Sub

   Public Shared Sub Enable(ByVal Controller As BaseController)
      For Each NodePath As String In Controller.SelectedPaths
         Controller.ApsimData.Find(NodePath).Enabled = True
      Next
   End Sub
   Public Shared Sub Disable(ByVal Controller As BaseController)
      For Each NodePath As String In Controller.SelectedPaths
         Controller.ApsimData.Find(NodePath).Enabled = False
      Next
   End Sub
   Public Shared Sub ToggleFactorialMode(ByVal Controller As BaseController)
        If (Not IsNothing(Controller.ApsimData.RootComponent)) Then
            Controller.FactorialMode = Not Controller.FactorialMode
        End If

    End Sub

   Private Shared ProgressBar As ToolStripProgressBar
   Private Shared ProgressLabel As ToolStripStatusLabel
   Public Shared Sub RunOnCluster(ByVal Controller As BaseController)
        If (Not (BaseActions.FileSave(Controller))) Then Return

        Dim StatusBar As StatusStrip = Controller.MainForm.Controls.Find("StatusStrip1", True)(0)
        ProgressBar = StatusBar.Items(0)
        ProgressLabel = StatusBar.Items(1)
        StatusBar.Visible = True

        Try
            Dim F As New UIBits.ClusterForm
            If F.ShowDialog = DialogResult.OK Then
                Cursor.Current = Cursors.WaitCursor
                Configuration.Instance.SetSetting("dropboxFolder", F.DropFolder)
                Configuration.Instance.SetSetting("dropboxApsimVersion", F.Version)
                If F.archIsUnix Then
                    Configuration.Instance.SetSetting("dropboxIsUnix", "true")
                Else
                    Configuration.Instance.SetSetting("dropboxIsUnix", "false")
                End If
                Configuration.Instance.SetSetting("dropboxSimsPerJob", F.simsPerJobNumber.ToString())

                Dim FilesToRun As New List(Of String)
                If F.FolderOfFiles = "" Then
                    If (Controller.ApsimData.FileName <> "") Then
                        FilesToRun.Add(Controller.ApsimData.FileName)
                    End If
                Else
                    Utility.FindFiles(F.FolderOfFiles, "*.apsim", FilesToRun, False)
                End If
                If (FilesToRun.Count > 0) Then
                    ToowoombaCluster.RunOnCluster(FilesToRun, F.DropFolder, F.Version, F.archIsUnix, F.simsPerJobNumber, AddressOf UpdateProgress)
                    MessageBox.Show("Your job has been placed in your dropbox folder. Your outputs will appear adjacent.", "For your information", MessageBoxButtons.OK, MessageBoxIcon.Information)
                End If
            End If
        Catch ex As Exception
            MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try
        Cursor.Current = Cursors.Default
        StatusBar.Visible = False
    End Sub

   Private Shared Sub UpdateProgress(ByVal Percent As Integer, ByVal Msg As String)
        ProgressBar.Value = Math.Min(100, Math.Max(0, Percent))
      ProgressLabel.Text = Msg
      Application.DoEvents()
   End Sub

#End Region

#Region "Output file methods"
   Private Declare Ansi Sub excelFiles Lib "ShellExtensions.dll" _
       Alias "excelFiles" (ByVal outFileList As String)
   Private Declare Ansi Sub apsvisFiles Lib "ShellExtensions.dll" _
           Alias "apsvisFiles" (ByVal outFileList As String)
   Private Declare Ansi Sub apsimoutlookFiles Lib "ShellExtensions.dll" _
           Alias "apsimoutlookFiles" (ByVal outFileList As String)

   Public Shared Sub Graph(ByVal Controller As BaseController)
      Dim FileNames As String = UIUtility.OutputFileUtility.GetCSVListOfOutputFiles(Controller)
      If FileNames = "" Then
         MessageBox.Show("No output files found")
      Else
         apsvisFiles(FileNames)
      End If
   End Sub

   Public Shared Sub ApsimOutlook(ByVal Controller As BaseController)
      Dim FileNames As String = UIUtility.OutputFileUtility.GetCSVListOfOutputFiles(Controller)
      If FileNames = "" Then
         MessageBox.Show("No output files found")
      Else
         apsimoutlookFiles(FileNames)
      End If
   End Sub

   Public Shared Sub Excel(ByVal Controller As BaseController)
      Dim FileNames As String = UIUtility.OutputFileUtility.GetCSVListOfOutputFiles(Controller)
      Dim exportChooser As ExcelExport = New ExcelExport
      If FileNames = "" Then
         Return
      End If

      Dim fileList() As String = FileNames.Split(",") 'break up the names into a list
      For Each s As String In fileList ' load the list into the dialog
         exportChooser.fileListBox.Items.Add(s, True)
      Next
      exportChooser.ShowDialog()
      FileNames = exportChooser.fileList
      excelFiles(FileNames)
   End Sub
#End Region

#Region "Checkpoint"
   Public Shared Sub CheckPoint(ByVal Controller As BaseController)
      If MessageBox.Show("Are you sure you want to save and checkpoint your simulation and outputfiles, overwriting any previous checkpoints?", _
                         "Are you sure?", MessageBoxButtons.YesNo, MessageBoxIcon.Exclamation) = DialogResult.Yes Then
         ' Save first
            If (Not (BaseActions.FileSave(Controller))) Then Return

         ' empty the checkpoint sub folder.
         Dim CheckPointFolder As String = Path.GetDirectoryName(Controller.ApsimData.FileName) + "\CheckPoint"

         If Directory.Exists(CheckPointFolder) Then
            Directory.Delete(CheckPointFolder, True)
         End If
         Directory.CreateDirectory(CheckPointFolder)

         ' Get a complete list of files names (.out, .sum & .apsim) to copy to checkpoint folder.
         Dim FileNames As New List(Of String)
         FileNames.Add(Controller.ApsimData.FileName)
         UIUtility.OutputFileUtility.GetOutputFiles(Controller, Controller.ApsimData.RootComponent, FileNames)
         UIUtility.OutputFileUtility.GetSummaryFiles(Controller, Controller.ApsimData.RootComponent, FileNames)

         ' Copy all files to checkpoint folder. If any files don't exist then 
         ' create zero byte files.
         For Each FileName As String In FileNames
            Dim DestFileName As String = CheckPointFolder + "\" + Path.GetFileName(FileName)
            If File.Exists(FileName) Then
               File.Copy(FileName, DestFileName, True)
            Else
               Dim Out As New StreamWriter(DestFileName)
               Out.Close()
            End If
         Next
         MessageBox.Show("All simulation, output and summary files have been checkpointed", "Success", MessageBoxButtons.OK, MessageBoxIcon.Information)
      End If
   End Sub

   Public Shared Sub RevertFromCheckPoint(ByVal Controller As BaseController)
      Dim CheckPointFolder As String = Path.GetDirectoryName(Controller.ApsimData.FileName) + "\CheckPoint"
      If Not Directory.Exists(CheckPointFolder) Then
         MessageBox.Show("No checkpoint found.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
      End If
      If MessageBox.Show("Are you sure you want to overwrite your current simulation, output and summary files with an earlier checkpoint?", _
                         "Are you sure?", MessageBoxButtons.YesNo, MessageBoxIcon.Exclamation) = DialogResult.Yes Then

         ' Restore all files.
         For Each FileName As String In Directory.GetFiles(CheckPointFolder)
            Dim DestFileName As String = Path.GetDirectoryName(Controller.ApsimData.FileName) + "\" + Path.GetFileName(FileName)
            File.Copy(FileName, DestFileName, True)
         Next
         Controller.SelectedPath = Controller.ApsimData.RootComponent.FullPath
         Controller.ApsimData.ReloadFromFile()
         Directory.Delete(CheckPointFolder, True)
      End If
    End Sub

    Public Shared Sub RemoveCheckpoint(ByVal Controller As BaseController)
        If MessageBox.Show("Are you sure you want to remove the current checkpoint? This will delete all files in the checkpoint folder.", _
                   "Are you sure?", MessageBoxButtons.YesNo, MessageBoxIcon.Exclamation) = DialogResult.Yes Then

            ' empty the checkpoint sub folder.
            Dim CheckPointFolder As String = Path.GetDirectoryName(Controller.ApsimData.FileName) + "\CheckPoint"
            If Directory.Exists(CheckPointFolder) Then
                Directory.Delete(CheckPointFolder, True)
            End If
        End If
    End Sub

#End Region

    Public Shared Sub ImportConFile(ByVal Controller As BaseController)
        If (Not (BaseActions.FileSave(Controller))) Then Return
        Dim F As New OpenFileDialog
        F.Filter = "Con files (*.con)|*.con|All files (*.*)|*.*"
        F.Title = "Select a .con file to import"
        If F.ShowDialog() = DialogResult.OK Then
            Dim NewXmlNode As XmlNode = ConToApsim.Converter.Go(F.FileName)
            If Not IsNothing(NewXmlNode) Then
                Controller.ApsimData.[New](NewXmlNode.OuterXml)
            End If
        End If
    End Sub

    Public Shared Sub Plant2Documentation(ByVal Controller As BaseController)
        If (Not (BaseActions.FileSave(Controller))) Then Return
        Dim XmlFileName As String = Controller.ApsimData.FileName
        'Dim HtmlFileName As String = Path.GetTempPath() + Path.GetFileNameWithoutExtension(XmlFileName) + ".html" //removed from temp dir as Firefox can't handle abs dirs. JF
        Dim HtmlFileName As String = "..\Documentation\Plant2Docs\" + Path.GetFileNameWithoutExtension(XmlFileName) + ".html"
        Dim Arguments As String = StringManip.DQuote(XmlFileName) + " " + StringManip.DQuote(HtmlFileName)

        'Dim P As Process = Process.Start(Configuration.ApsimBinDirectory + "\Plant2Documentation", Arguments)
        Dim P As Process = Utility.RunProcess(Configuration.ApsimBinDirectory + "\Plant2Documentation.exe", Arguments, Path.GetDirectoryName(XmlFileName))
        Utility.CheckProcessExitedProperly(P)
        Process.Start(HtmlFileName)
    End Sub

    Public Shared Sub ProbeDLL(ByVal Controller As BaseController)
        If (Not (BaseActions.FileSave(Controller))) Then Return
        Dim XmlFileName As String = Controller.ApsimData.FileName

        'Dim P As Process = Process.Start(Configuration.ApsimBinDirectory + "\Plant2Documentation", Arguments)
        Dim P As Process = Utility.RunProcess(Configuration.ApsimBinDirectory + "\ProbeDLL.exe", XmlFileName, Path.GetDirectoryName(XmlFileName))
        Utility.CheckProcessExitedProperly(P)

        MessageBox.Show("Finished writing to <info> section.", "Done", MessageBoxButtons.OK, MessageBoxIcon.Information)
    End Sub


    Public Shared Sub CreateDuplicates(ByVal Controller As BaseController)
        Dim f As New UIBits.DuplicateForm
        If f.ShowDialog = DialogResult.OK Then
            Dim Comp As ApsimFile.Component = Controller.Selection

            For i As Integer = 1 To f.NumDuplicates
                Comp.Parent.Duplicate(Comp, f.DoLinkDuplicates)
            Next

        End If
    End Sub


    Public Shared Sub LinkWherePossible(ByVal Controller As BaseController)
        Cursor.Current = Cursors.WaitCursor
        Dim Base As ApsimFile.Component = Controller.Selection
        Dim Siblings As New List(Of ApsimFile.Component)
        Base.Parent.FindRecursively(Base.Type, Siblings)

        For Each Sibling As ApsimFile.Component In Siblings
            If Sibling.FullPath <> Base.FullPath Then
                Sibling.ConvertToShortcutsUsingBase(Base)
            End If
        Next
        Controller.ApsimData.PublishComponentChanged(Controller.ApsimData.RootComponent)

        Cursor.Current = Cursors.Default
    End Sub
End Class





