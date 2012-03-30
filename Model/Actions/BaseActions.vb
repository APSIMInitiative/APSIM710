
Imports System.Collections.Specialized
Imports System.IO
Imports System.Windows.Forms
Imports System.Xml
Imports System.Drawing

Imports ApsimFile
Imports Controllers
Imports CSGeneral
Imports UIBits              'InputDialog
Imports System.Drawing.Printing



Public Class BaseActions
    Public Shared Sub FileOpen(ByVal Controller As BaseController)
        Dim LastDir As String
        If Controller.FileSaveAfterPrompt() Then
            If (Configuration.Instance.GetFrequentList.Count() > 0) Then
                LastDir = Configuration.Instance.GetFrequentList()(0)
            Else
                LastDir = Environment.GetFolderPath(Environment.SpecialFolder.Personal)
            End If

            Dim dialog As New OpenFileDialog
            If LastDir <> "" Then
                LastDir = LastDir.Substring(0, LastDir.LastIndexOf("\"))
                dialog.InitialDirectory = LastDir
            End If

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
   Public Shared Function FileSave(ByVal Controller As BaseController)
      If Controller.ApsimData.FileName = "Untitled" Then
         Return BaseActions.FileSaveAs(Controller)
      Else
         Return Controller.ApsimData.Save()
      End If
   End Function
   Public Shared Function FileSaveAs(ByVal Controller As BaseController) As Boolean
      Dim result As Boolean = False
        Dim Dialog As New SaveFileDialog
        Dim LastDir As String
        If (Configuration.Instance.GetFrequentList.Count > 0) Then
            LastDir = Configuration.Instance.GetFrequentList()(0)
            LastDir = LastDir.Substring(0, LastDir.LastIndexOf("\"))
        Else
            LastDir = Environment.GetFolderPath(Environment.SpecialFolder.Personal)
        End If
        Dialog.InitialDirectory = LastDir
        Dialog.Filter = Configuration.Instance.Setting("DialogFilter")
        Dialog.DefaultExt = Configuration.Instance.Setting("DefaultExtension")
        Dialog.AddExtension = True
        Dialog.OverwritePrompt = True
        If Dialog.ShowDialog = DialogResult.OK Then
            Controller.Explorer.SaveCurrentView()
            result = Controller.ApsimData.SaveAs(Dialog.FileName)
            Controller.RefreshToolStrips()
        End If
        Return result
    End Function
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
                    MessageBox.Show("You can not use characters such as < > / \ ' "" ` : ? | * & = ! . , or space in the name")
            End If

         End If

      Next
   End Sub
   Public Shared Sub DeleteFactor(ByVal Controller As BaseController)
      ' --------------------------------------------------------
      ' Delete selected nodes
      ' --------------------------------------------------------
      Dim CompToDelete As ApsimFile.Component = Controller.FactorialSelection
        'if this is the fatorial node and there is a project loaded, then you cannot delete it
        If IsNothing(CompToDelete.Parent) Or IsNothing(CompToDelete.Parent.Parent) Then
            Return
        End If

        ' find next sibling, or previous sibling, or parent the set SelectedFactorialPath
        Dim CompToSelect As ApsimFile.Component = CompToDelete.Parent

        Controller.SelectedFactorialPath = CompToSelect.FullPath
        CompToDelete.Parent.Delete(CompToDelete)

        Controller.Explorer.RefreshCurrentView()
    End Sub
   Public Shared Sub AddFactorFolder(ByVal Controller As BaseController)
      ' --------------------------------------------------------
      ' Add a folder
      ' --------------------------------------------------------
      Controller.FactorialSelection.Add("<folder/>")
   End Sub
   Public Shared Sub AddFactor(ByVal Controller As BaseController)
      ' --------------------------------------------------------
      ' Add a folder
      ' --------------------------------------------------------
        Controller.FactorialSelection.Add("<factor/>")

   End Sub

#Region "Printing"
   Private Shared ComponentsToPrint As New List(Of String)
   Private Shared ComponentsToPrintIndex As Integer
   Private Shared Contr As BaseController
   Private Shared PrintSettings As Printing.PrinterSettings = Nothing

   Public Shared Sub Print(ByVal Controller As BaseController)

      Dim pd As New PrintDocument()
      AddHandler pd.PrintPage, AddressOf OnPrintPage

      Dim PreviewDialog As New UIBits.PrintPreviewDialog()
      PreviewDialog.Document = pd

      ComponentsToPrint.Clear()
      GetListOfComponentsToPrint(Controller, Controller.Selection)
      ComponentsToPrintIndex = -1
      Contr = Controller
      PreviewDialog.ShowDialog()
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

        ' go export all graphs that we can find.
      ExportAllRecursively(Selection, ExportDirectory, ExportExtension)

   End Sub

   Private Shared Sub ExportAllRecursively(ByVal Component As Component, _
                                           ByVal ExportDirectory As String, _
                                           ByVal ExportExtension As String)
      If (Component.Type = "Graph" Or Component.Type = "Graph2" Or Component.Type = "RegressionGraph" Or Component.Type = "GraphReport" Or Component.Type = "memo") Then
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
      g.FillRectangle(Brushes.White, r)

      Contr.Explorer.CurrentView.PrintPage(r, g)

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

      Dim Doc As New XmlDocument
      Dim Root As XmlNode = Doc.AppendChild(Doc.CreateElement("dummy"))
      For Each ComponentPath As String In Controller.SelectedPaths
         Dim Component As Component = Controller.ApsimData.Find(ComponentPath)
         If Not IsNothing(Component) Then
            Dim Node As XmlNode = Root.AppendChild(Doc.CreateElement(Component.Type))
            Component.Write(Node)
         End If

      Next
      Clipboard.SetDataObject(Root.InnerXml, True)
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



