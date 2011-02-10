
Imports System.Collections.Specialized
Imports System.IO
Imports System.Windows.Forms
Imports System.Xml
Imports System.Drawing

Imports ApsimFile
Imports Controllers
Imports CSGeneral
Imports UIBits              'InputDialog
Imports TMGDevelopment.Windows.Forms



Public Class BaseActions
    Public Shared Sub FileOpen(ByVal Controller As BaseController)
        If Controller.FileSaveAfterPrompt() Then
            Dim dialog As New OpenFileDialog
            dialog.Filter = Configuration.Instance.Setting("DialogFilter")
            dialog.DefaultExt = Configuration.Instance.Setting("DefaultExtension")
            dialog.RestoreDirectory = True
            If dialog.ShowDialog = DialogResult.OK Then
                Controller.Explorer.CloseUI()
                Controller.ApsimData.OpenFile(dialog.FileName)
                Controller.RefreshToolStrips()
            End If
        End If
    End Sub
    Public Shared Sub FileSave(ByVal Controller As BaseController)
        If Controller.ApsimData.FileName = "Untitled" Then
            BaseActions.FileSaveAs(Controller)
        Else
            Controller.ApsimData.Save()
        End If
    End Sub
    Public Shared Sub FileSaveAs(ByVal Controller As BaseController)
        Dim Dialog As New SaveFileDialog
        Dialog.Filter = Configuration.Instance.Setting("DialogFilter")
        Dialog.DefaultExt = Configuration.Instance.Setting("DefaultExtension")
        Dialog.AddExtension = True
        Dialog.OverwritePrompt = True
        If Dialog.ShowDialog = DialogResult.OK Then
            Controller.Explorer.SaveCurrentView()
            Controller.ApsimData.SaveAs(Dialog.FileName)
            Controller.RefreshToolStrips()
        End If
    End Sub
    Public Shared Sub HelpAbout(ByVal Controller As BaseController)
        If Configuration.Instance.Setting("SplashScreen") <> "" Then
            Dim SplashForm As Form = BaseController.CreateClass(Configuration.Instance.Setting("SplashScreen"))
            SplashForm.ShowDialog()
        End If
    End Sub
    Public Shared Sub AddFolder(ByVal Controller As BaseController)
        ' --------------------------------------------------------
        ' Add a folder
        ' --------------------------------------------------------
        Controller.Selection.Add("<folder/>")
    End Sub
    Public Shared Sub Delete(ByVal Controller As BaseController)
        ' --------------------------------------------------------
        ' Delete selected nodes
        ' --------------------------------------------------------
        Dim PathsToDelete As Specialized.StringCollection = Controller.SelectedPaths

        Dim ParentSelection As String = Controller.SelectedPaths(0)
        For Each SelectedPath As String In PathsToDelete
            If ParentSelection.IndexOf(SelectedPath) = 0 Then
                ParentSelection = SelectedPath
            End If
        Next
        Dim Selection As ApsimFile.Component = Controller.ApsimData.Find(ParentSelection)
        Controller.SelectedPath = Selection.Parent.FullPath

        For Each SelectedPath As String In PathsToDelete
            Dim CompToDelete As ApsimFile.Component = Controller.ApsimData.Find(SelectedPath)
            CompToDelete.Parent.Delete(CompToDelete)
        Next
        Controller.Explorer.RefreshCurrentView()
    End Sub
    Public Shared Sub Rename(ByVal Controller As BaseController)
        ' --------------------------------------------------------
        ' Rename selected nodes
        ' --------------------------------------------------------
        For Each SelectedPath As String In Controller.SelectedPaths

            'get the new name the user entered
            Dim NewName = UIBits.InputDialog.InputBox("Enter new name for node:", "Rename the selected node", Controller.Selection.Name, False)

            'set rename the selected node
            If NewName <> "" Then

                If Not CSGeneral.Utility.CheckForInvalidChars(NewName) Then
                    Dim Comp As ApsimFile.Component = Controller.ApsimData.Find(SelectedPath)
                    Comp.Name = NewName
                    ' Now tell the base controller about the new selections.
                    Controller.SelectedPath = Comp.FullPath
                Else
                    MessageBox.Show("You can not use characters such as < > / \ ' "" ` : ? | * & = ! in the name")
                End If

            End If

        Next
    End Sub

#Region "Printing"
    Private Shared ComponentsToPrint As New List(Of String)
    Private Shared ComponentsToPrintIndex As Integer
    Private Shared Contr As BaseController
    Private Shared PrintForm As TMGDevelopment.Windows.Forms.PrintForm = Nothing
    Private Shared PrintSettings As Printing.PrinterSettings = Nothing

    Public Shared Sub Print(ByVal Controller As BaseController)
        PrintForm = New TMGDevelopment.Windows.Forms.PrintForm(Nothing)
        If PrintSettings Is Nothing Then
            PrintSettings = PrintForm.PrinterSettings
        Else
            PrintForm.PrinterSettings = PrintSettings
        End If
        PrintForm.BodyContainer = Controller.Explorer.CurrentView
        Dim PreviewDialog As New UIBits.PrintPreviewDialog()
        PreviewDialog.Document = PrintForm

        AddHandler PrintForm.PreDraw, AddressOf OnPreDraw
        AddHandler PrintForm.PrintPage, AddressOf OnPrintPage

        ComponentsToPrint.Clear()
        GetListOfComponentsToPrint(Controller, Controller.Selection)
        ComponentsToPrintIndex = -1
        Contr = Controller
        PreviewDialog.ShowDialog()
    End Sub

    Private Shared Sub OnPreDraw(ByVal sender As System.Object, ByVal e As TMGDevelopment.Windows.Forms.PreDrawEventArgs)
        e.OwnerDrawn = True
    End Sub

    Private Shared Sub OnPrintPage(ByVal sender As System.Object, ByVal e As System.Drawing.Printing.PrintPageEventArgs)
        ComponentsToPrintIndex = ComponentsToPrintIndex + 1
        If ComponentsToPrintIndex < ComponentsToPrint.Count Then
            Contr.SelectedPath = ComponentsToPrint(ComponentsToPrintIndex)
            Contr.Explorer.CurrentView.PrintPage(e.MarginBounds, e.Graphics)
        Else
            e.Cancel = True
        End If
        e.HasMorePages = (ComponentsToPrintIndex + 1 < ComponentsToPrint.Count)
        If Not e.HasMorePages Then
            ComponentsToPrintIndex = -1
        End If
    End Sub


    Private Shared Sub GetListOfComponentsToPrint(ByVal Controller As BaseController, ByVal Component As ApsimFile.Component)
        If (Component.Type = "Graph" Or Component.Type = "RegressionGraph" Or Component.Type = "GraphReport" Or Component.Type = "memo") Then
            ComponentsToPrint.Add(Component.FullPath)
        Else
            For Each Child As ApsimFile.Component In Component.ChildNodes
                GetListOfComponentsToPrint(Controller, Child)
            Next
        End If
    End Sub
#End Region

#Region "Export"

    Public Shared Sub ExportToBMP(ByVal Controller As BaseController)
        Export(Controller, ".bmp")
    End Sub
    Public Shared Sub ExportToGIF(ByVal Controller As BaseController)
        Export(Controller, ".gif")
    End Sub
    Public Shared Sub ExportToJPG(ByVal Controller As BaseController)
        Export(Controller, ".jpg")
    End Sub
    Public Shared Sub ExportToPNG(ByVal Controller As BaseController)
        Export(Controller, ".png")
    End Sub
    Public Shared Sub Export(ByVal Controller As BaseController, ByVal Extension As String)
        Dim FolderDialog As New FolderBrowserDialog()
        If FolderDialog.ShowDialog() = DialogResult.OK Then
            ComponentsToPrint.Clear()
            Contr = Controller

            ExportAll(Contr, Contr.Selection, FolderDialog.SelectedPath, Extension)

        End If
        MessageBox.Show("All graphs have been exported", "Export completed", MessageBoxButtons.OK, MessageBoxIcon.Information)
    End Sub


    Public Shared Sub ExportAll(ByVal Controller As BaseController, ByVal Selection As Component, _
                                ByVal ExportDirectory As String, ByVal ExportExtension As String)
        Contr = Controller

        ' cleanup all previous files.
        Utility.DeleteFiles(ExportDirectory + "\\*" + ExportExtension, True)

        ' go export all graphs that we can find.
        ExportAllRecursively(Selection, ExportDirectory, ExportExtension)

    End Sub

    Private Shared Sub ExportAllRecursively(ByVal Component As Component, _
                                            ByVal ExportDirectory As String, _
                                            ByVal ExportExtension As String)
        If (Component.Type = "Graph" Or Component.Type = "RegressionGraph" Or Component.Type = "GraphReport" Or Component.Type = "memo") Then
            ExportComponent(Component.FullPath, ExportDirectory, ExportExtension)

        ElseIf Component.Type = "area" Or _
                Component.Type = "simulation" Or _
                Component.Type = "folder" Or _
                Component.Type = "outputfile" Then
            ' Need to recurse.
         If Component.Type = "folder" Or Component.Type = "simulation" Then
            ExportDirectory = ExportDirectory + "\" + Component.Name
         End If
            For Each Child As ApsimFile.Component In Component.ChildNodes
                ExportAllRecursively(Child, ExportDirectory, ExportExtension)
            Next
        End If

    End Sub

    Private Shared Sub ExportComponent(ByVal SelectedPath As String, _
                                       ByVal ExportDirectory As String, _
                                       ByVal ExportExtension As String)
        Directory.CreateDirectory(ExportDirectory)

        Contr.SelectedPath = SelectedPath
        Dim CurrentView As BaseView = Contr.Explorer.CurrentView
        Dim r As New Rectangle(New Point(0, 0), CurrentView.Size)
        Dim img As New Bitmap(r.Width, r.Height)
        Dim g As Graphics = Graphics.FromImage(img)

        ' change the controller to the standard one so that we don't get a 'printing page...' dialog.
        ' PrintController OldController = PrintForm.PrintController;
        Dim PF As New PrintForm()
        PF.PrintController = New System.Drawing.Printing.StandardPrintController()
        PF.BodyContainer = CurrentView
        AddHandler PF.PreDraw, AddressOf OnExportPreDraw
        PF.PrintControl(g, r, CurrentView, 1.0F)

        g.Dispose()
        Dim NewFileName As String = ExportDirectory + "\\" _
                                  + Contr.ApsimData.Find(SelectedPath).Name _
                                  + ExportExtension
        If (Path.GetExtension(NewFileName) = ".bmp") Then
            img.Save(NewFileName, System.Drawing.Imaging.ImageFormat.Bmp)
        ElseIf (Path.GetExtension(NewFileName) = ".gif") Then
            img.Save(NewFileName, System.Drawing.Imaging.ImageFormat.Gif)
        ElseIf (Path.GetExtension(NewFileName) = ".jpg") Then
            img.Save(NewFileName, System.Drawing.Imaging.ImageFormat.Jpeg)
        ElseIf (Path.GetExtension(NewFileName) = ".png") Then
            img.Save(NewFileName, System.Drawing.Imaging.ImageFormat.Png)
        Else
            Throw New Exception("Invalid format for exporting: " + NewFileName)
            img.Dispose()
        End If
    End Sub

    Private Shared Sub OnExportPreDraw(ByVal sender As Object, ByVal e As PreDrawEventArgs)
        Contr.Explorer.CurrentView.PrintPage(e.Bounds, e.Graphics)
        e.OwnerDrawn = True
    End Sub

#End Region


    Public Shared Sub Cut(ByVal Controller As BaseController)
        ' --------------------------------------------------------
        ' Perform a clipboard cut operation
        ' --------------------------------------------------------
        Copy(Controller)
        Delete(Controller)
    End Sub
    Public Shared Sub Copy(ByVal Controller As BaseController)
        ' --------------------------------------------------------
        ' Perform a clipboard copy operation
        ' --------------------------------------------------------
        Controller.ApsimData.CopyToClipboard(Controller.SelectedPaths)
    End Sub
    Public Shared Sub Paste(ByVal Controller As BaseController)
        ' --------------------------------------------------------
        ' Perform a clipboard paste operation
        ' --------------------------------------------------------
        Dim iData As IDataObject = Clipboard.GetDataObject()
        Dim xml As String = CType(iData.GetData(DataFormats.Text), String)
        Controller.Selection.Add(xml)
    End Sub
    Public Shared Sub MoveUp(ByVal Controller As BaseController)
        ' --------------------------------------------------------        
        ' Move all selected items up
        ' --------------------------------------------------------
        Dim PathsToMove As New List(Of String)
        For Each SelectedPath As String In Controller.SelectedPaths
            PathsToMove.Add(SelectedPath.Substring(SelectedPath.LastIndexOf(XmlHelper.Delimiter) + 1))
        Next
        Dim Parent As ApsimFile.Component = Controller.ApsimData.Find(Controller.SelectedPaths(0)).Parent
        Parent.MoveUp(PathsToMove)
    End Sub
    Public Shared Sub MoveDown(ByVal Controller As BaseController)
        ' --------------------------------------------------------        
        ' Move all selected items down
        ' --------------------------------------------------------
        Dim PathsToMove As New List(Of String)
        For Each SelectedPath As String In Controller.SelectedPaths
            PathsToMove.Add(SelectedPath.Substring(SelectedPath.LastIndexOf(XmlHelper.Delimiter) + 1))
        Next
        Dim Parent As ApsimFile.Component = Controller.ApsimData.Find(Controller.SelectedPaths(0)).Parent
        Parent.MoveDown(PathsToMove)
    End Sub

    Public Shared Sub ExpandAll(ByVal Controller As BaseController)
        ' --------------------------------------------------------
        ' Expand all nodes in tree.
        ' --------------------------------------------------------
        Controller.Explorer.ExpandAll()
    End Sub
    Public Shared Sub CollapseAll(ByVal Controller As BaseController)
        ' --------------------------------------------------------
        ' Collapse all nodes in tree.
        ' --------------------------------------------------------
        Controller.Explorer.CollapseAll()
    End Sub


   Public Shared Sub Unlink(ByVal Controller As BaseController)
      Controller.Selection.MakeConcrete()
   End Sub

   Public Shared Sub UnlinkRecursively(ByVal Controller As BaseController)
      Controller.Selection.MakeConcreteRecursively()
   End Sub

End Class



