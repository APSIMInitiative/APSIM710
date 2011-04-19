
Imports System
Imports System.Collections.Generic
Imports System.Drawing
Imports System.IO
Imports System.Net
Imports System.Reflection
Imports System.Text
Imports System.Windows.Forms
Imports System.Xml

Imports ApsimFile
Imports Controllers
Imports CSGeneral
Imports CSUserInterface 'SoilUI
Imports UIBits          'ErrorMessageForm
Imports UIUtility
Imports ExcelUtility


Public Class SoilActions

    Public Shared Sub FileNew(ByVal Controller As BaseController)
        If Controller.FileSaveAfterPrompt() Then
            Dim Dialog As New SaveFileDialog()
            Dialog.Filter = "Soils files (*.soils)|*.soils|" + "All files (*.*)|*.*"
            Dialog.DefaultExt = "soils"
            Dialog.Title = "Provide a filename to save the new soils file to"
            If Dialog.ShowDialog() = DialogResult.OK Then
                Dim Out As New StreamWriter(Dialog.FileName)
                Out.WriteLine("<folder name=""Soils"" version=""" + ApsimFile.APSIMChangeTool.CurrentVersion.ToString() + """/>")
                Out.Close()
                Controller.ApsimData.OpenFile(Dialog.FileName)
            End If
        End If
    End Sub
   Public Shared Sub AddSoil(ByVal Controller As BaseController)
      Dim NewSoil As XmlNode = Soil.Create("Soil")
      Controller.Selection.Add(NewSoil.OuterXml)
   End Sub




#Region "Import methods"
    Public Shared Sub ImportFromSoils(ByVal Controller As BaseController)
        Dim Dialog As New OpenFileDialog()
        Dialog.Filter = "Soils files (*.soils)|*.soils|All files (*.*)|*.*"
        Dialog.Title = "Select 1 or more .soils file(s) to import from"
        Dialog.Multiselect = True
        Dialog.RestoreDirectory = True
        If Dialog.ShowDialog() = DialogResult.OK Then
            For Each FileName As String In Dialog.FileNames
                Dim Doc As New XmlDocument()
                Doc.Load(FileName)
                APSIMChangeTool.Upgrade(Doc.DocumentElement)
                Controller.Selection.Add(Doc.DocumentElement.InnerXml)
            Next
        End If
    End Sub
    Public Shared Sub ImportFromPar(ByVal Controller As BaseController)
        Dim Dialog As New OpenFileDialog()
        Dialog.Filter = "Par files (*.par)|*.par|All files (*.*)|*.*"
        Dialog.Title = "Select 1 or more .par file(s) to import from"
        Dialog.Multiselect = True
        Dialog.RestoreDirectory = True
        If Dialog.ShowDialog() = DialogResult.OK Then
            For Each FileName As String In Dialog.FileNames
                Cursor.Current = Cursors.WaitCursor
                Controller.Selection.Add(SoilParFileImporter.Import(FileName))
                Cursor.Current = Cursors.Default
            Next
        End If
   End Sub

   Private Shared ProgressBar As ToolStripProgressBar

   Public Shared Sub FileOpenXLS(ByVal Controller As BaseController)
      Dim StatusBar As StatusStrip = Controller.MainForm.Controls.Find("StatusStrip1", True)(0)
      ProgressBar = StatusBar.Items(0)
      StatusBar.Visible = True

      Dim Dialog As New OpenFileDialog()
      Dialog.Filter = "Spreadsheet files (*.xls)|*.xls|All files (*.*)|*.*"
      Dialog.Title = "Select a spreadsheet to open"
      Dialog.RestoreDirectory = True
      If Dialog.ShowDialog() = DialogResult.OK Then
         Cursor.Current = Cursors.WaitCursor
         Dim TempFileName As String = SoilSpreadsheet.OpenXLS(Dialog.FileName, AddressOf UpdateProgress)
         Controller.ApsimData.NewFromFile(TempFileName)
         File.Delete(TempFileName)
         Cursor.Current = Cursors.Default
      End If
      StatusBar.Visible = False
   End Sub

   Private Shared Sub UpdateProgress(ByVal Percent As Integer)
      ProgressBar.Value = Percent
      Application.DoEvents()
   End Sub

#End Region



#Region "Export methods"

    Public Shared Sub ExportToSoils(ByVal Controller As BaseController)
        Dim Dialog As New SaveFileDialog()
        Dialog.Filter = "Soils files (*.soils)|*.soils|All files (*.*)|*.*"
        Dialog.Title = "Enter a .soils file to export to"
        Dialog.DefaultExt = "soils"
        If Dialog.ShowDialog() = DialogResult.OK Then
            Dim Doc As New XmlDocument()
            If Not File.Exists(Dialog.FileName) Then
                Doc.AppendChild(XmlHelper.CreateNode(Doc, "soils", ""))
            Else
                Doc.Load(Dialog.FileName)
            End If

            For Each SelectedPath As String In Controller.SelectedPaths
                Dim Comp As ApsimFile.Component = Controller.ApsimData.Find(SelectedPath)
                Dim NodeDoc As New XmlDocument()
            NodeDoc.LoadXml(Comp.FullXML())
                Doc.DocumentElement.AppendChild(Doc.ImportNode(NodeDoc.DocumentElement, True))
            Next
            XmlHelper.SetAttribute(Doc.DocumentElement, "version", ApsimFile.APSIMChangeTool.CurrentVersion.ToString())
            Doc.Save(Dialog.FileName)
            MessageBox.Show("Soils have been successfully exported to '" + Dialog.FileName + "'. It is suggested that you rename soils within the new file to avoid confusion.", "Success", MessageBoxButtons.OK, MessageBoxIcon.Information)
        End If
    End Sub
   Public Shared Sub ExportToSpreadsheet(ByVal Controller As BaseController)
      Dim Dialog As New SaveFileDialog()
      Dialog.Filter = "Spreadsheet files (*.xls)|*.xls|All files (*.*)|*.*"
      Dialog.Title = "Enter a spreadsheet file to export to"
      Dialog.DefaultExt = "xls"
      If Dialog.ShowDialog() = DialogResult.OK Then
         Dim Doc As New XmlDocument
         Doc.LoadXml(Controller.ApsimData.RootComponent.FullXML())
         SoilSpreadsheet.Export(Dialog.FileName, Doc.DocumentElement, Controller.SelectedPaths)
         MessageBox.Show("Soils have been successfully exported to '" + Dialog.FileName + "'. It is suggested that you rename soils within the new file to avoid confusion.", "Success", MessageBoxButtons.OK, MessageBoxIcon.Information)
      End If
   End Sub

#End Region



    Public Shared Sub CheckSoils(ByVal Controller As BaseController)
        ' User wants to check all soils for consistency 
        Cursor.Current = Cursors.WaitCursor
        Dim ErrorMessage As String = ""
        For Each SelectedPath As String In Controller.SelectedPaths
            CheckSoils(Controller.ApsimData.Find(SelectedPath), ErrorMessage)
        Next
        If ErrorMessage = "" Then
            MessageBox.Show("All soils checked out ok. No problems were encountered", "No problems encountered", MessageBoxButtons.OK, MessageBoxIcon.Information)
        Else
            Dim ErrorForm As New UIBits.ErrorMessageForm()
            ErrorForm.SetText(ErrorMessage)
            ErrorForm.Show()
        End If
        Cursor.Current = Cursors.[Default]
    End Sub
    Private Shared Sub CheckSoils(ByVal Data As ApsimFile.Component, ByRef ErrorMessage As String)
        If Data.Type.ToLower() = "soil" Then
         Dim ThisSoil As XmlNode = Soil.CreateFromXML(Data.FullXML())
         Dim Errors As String = Soil.CheckForErrors(ThisSoil, True)
            If Errors <> "" Then
            ErrorMessage += vbCr + vbLf + XmlHelper.Name(ThisSoil) + vbCr + vbLf + StringManip.IndentText(Errors, 6)
            End If
        ElseIf Data.Type.ToLower() = "folder" Then
            For Each Child As ApsimFile.Component In Data.ChildNodes
                CheckSoils(Child, ErrorMessage)
            Next
        End If
    End Sub

    Public Shared Sub SortSoils(ByVal Controller As BaseController)
        Cursor.Current = Cursors.WaitCursor
        Controller.Selection.Sort()
        Cursor.Current = Cursors.[Default]
    End Sub


    Public Shared Sub Version(ByVal Controller As BaseController)
        MessageBox.Show(VersionString(), "Apsoil version", MessageBoxButtons.OK, MessageBoxIcon.Information)
    End Sub
    Public Shared Function VersionString() As String
        Return "Version " + Configuration.Instance.ApsimVersion()
    End Function
   Public Shared Sub OpenLatestVersionOfSoilsDatabase(ByVal Controller As BaseController)
      If Controller.FileSaveAfterPrompt() Then

         Cursor.Current = Cursors.WaitCursor
         Dim request As WebRequest = WebRequest.Create("http://www.apsim.info/Wiki/public/Upload/ApSoil/APSRU-Australia-Soils.soils")
         Dim response As HttpWebResponse = DirectCast(request.GetResponse(), HttpWebResponse)
         If response.StatusDescription = "OK" Then
            Dim dataStream As Stream = response.GetResponseStream()
            Dim reader As New StreamReader(dataStream)
            Dim responseFromServer As String = reader.ReadToEnd()

            Dim SoilsFileName As String = Path.GetTempFileName()
            Dim SoilsFile As New StreamWriter(SoilsFileName)
            SoilsFile.Write(responseFromServer)
            SoilsFile.Close()
            reader.Close()
            dataStream.Close()
            Controller.ApsimData.NewFromFile(SoilsFileName)
            File.Delete(SoilsFileName)
         Else
            MessageBox.Show("Cannot connect to www.apsim.info", "Failure", MessageBoxButtons.OK, MessageBoxIcon.[Error])
         End If
         response.Close()
         Cursor.Current = Cursors.[Default]
      End If
   End Sub
    Public Shared Sub ReleaseNotes(ByVal Controller As BaseController)
        Dim URL As String = Configuration.Instance.Setting("ReleaseNotes")
        System.Diagnostics.Process.Start(URL)
    End Sub

   Public Shared Sub GoogleEarthSoils(ByVal Controller As BaseController)
      If Controller.FileSaveAfterPrompt() Then
         Cursor.Current = Cursors.WaitCursor
         Dim request As WebRequest = WebRequest.Create("http://www.apsim.info/ApsoilWeb/ApsoilKML.aspx")
         Dim response As HttpWebResponse = DirectCast(request.GetResponse(), HttpWebResponse)
         If response.StatusDescription = "OK" Then
            Dim dataStream As Stream = response.GetResponseStream()
            Dim reader As New StreamReader(dataStream)
            Dim responseFromServer As String = reader.ReadToEnd()

            Dim KMLFileName As String = Path.GetTempPath() + "Soils.kmz"
            Dim KMLFile As New StreamWriter(KMLFileName)
            KMLFile.Write(responseFromServer)
            KMLFile.Close()
            reader.Close()
            dataStream.Close()
            Dim P As Process = CSGeneral.Utility.RunProcess(KMLFileName, "", Path.GetTempPath())
            Dim Errors As String = CSGeneral.Utility.CheckProcessExitedProperly(P)
            If Errors <> "" Then
               MessageBox.Show(Errors, "Error", MessageBoxButtons.OK, MessageBoxIcon.[Error])
            End If

         Else
            MessageBox.Show("Cannot connect to www.apsim.info", "Failure", MessageBoxButtons.OK, MessageBoxIcon.[Error])
         End If
         response.Close()
         Cursor.Current = Cursors.[Default]
      End If
   End Sub



#Region "PrintSoil"
    Public Shared Sub PrintSoil(ByVal Controller As BaseController)
        'Dim SoilUI As CSUserInterface.SoilUI = DirectCast(Controller.Explorer.CurrentView, CSUserInterface.SoilUI)

        'Dim PrintDocument As New System.Drawing.Printing.PrintDocument()
        'AddHandler PrintDocument.BeginPrint, AddressOf SoilUI.OnBeginPrint
        'AddHandler PrintDocument.PrintPage, AddressOf SoilUI.OnPrintPage
        'AddHandler PrintDocument.QueryPageSettings, AddressOf OnQueryPageSettings

        'Dim PrintDialog As New PrintDialog()
        'PrintDialog.Document = PrintDocument

        'Dim PreviewDialog As New System.Windows.Forms.PrintPreviewDialog()
        'PreviewDialog.Document = PrintDocument
        'PreviewDialog.ShowDialog()
    End Sub
    Private Shared Sub OnQueryPageSettings(ByVal sender As Object, ByVal e As System.Drawing.Printing.QueryPageSettingsEventArgs)
        e.PageSettings.Margins.Left = 50
        e.PageSettings.Margins.Top = 50
        e.PageSettings.Margins.Right = 50
        e.PageSettings.Margins.Bottom = 50
    End Sub
#End Region


End Class




