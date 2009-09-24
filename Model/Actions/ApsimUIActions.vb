
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
        BaseActions.FileSave(Controller)
        Dim RunPanels As Control() = Controller.MainForm.Controls.Find("RunToolStrip", True)
        If RunPanels.Length = 1 Then
            ApsimRunToolStrip.Instance.RunApsim(RunPanels(0), _
                                                Controller.ApsimData, _
                                                Controller.SelectedPaths)
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
        If FileNames = "" Then
            MessageBox.Show("No output files found")
        Else
            excelFiles(FileNames)
        End If
    End Sub
#End Region

#Region "Checkpoint"
    Public Shared Sub CheckPoint(ByVal Controller As BaseController)
        If MessageBox.Show("Are you sure you want to checkpoint your simulation and outputfiles, overwriting any previous checkpoints?", _
                           "Are you sure?", MessageBoxButtons.YesNo, MessageBoxIcon.Exclamation) = DialogResult.Yes Then

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

#End Region
End Class


