
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
        Controller.Selection.Add("<soil name=""NewSoil""/>")
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
    Public Shared Sub ImportFromSpreadsheet(ByVal Controller As BaseController)
        Dim Dialog As New OpenFileDialog()
        Dialog.Filter = "Spreadsheet files (*.xls)|*.xls|All files (*.*)|*.*"
        Dialog.Title = "Select a spreadsheet to import from"
        Dialog.RestoreDirectory = True
        If Dialog.ShowDialog() = DialogResult.OK Then
            Cursor.Current = Cursors.WaitCursor
            Controller.Selection.Add(SoilSpreadsheet.Import(Dialog.FileName), False)
            Cursor.Current = Cursors.Default
        End If
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
                NodeDoc.LoadXml(Comp.Contents)
                Doc.DocumentElement.AppendChild(Doc.ImportNode(NodeDoc.DocumentElement, True))
            Next
            XmlHelper.SetAttribute(Doc.DocumentElement, "version", ApsimFile.APSIMChangeTool.CurrentVersion.ToString())
            Doc.Save(Dialog.FileName)
            MessageBox.Show("Soils have been successfully exported to '" + Dialog.FileName + "'. It is suggested that you rename soils within the new file to avoid confusion.", "Success", MessageBoxButtons.OK, MessageBoxIcon.Information)
        End If
    End Sub
    Public Shared Sub ExportToPar(ByVal Controller As BaseController)
        Dim Dialog As New SaveFileDialog()
        Dialog.Filter = "Par files (*.par)|*.par|All files (*.*)|*.*"
        Dialog.Title = "Enter a .par file to export to"
        Dialog.DefaultExt = "par"
        If Dialog.ShowDialog() = DialogResult.OK Then
            File.Delete(Dialog.FileName)
            For Each SelectedPath As String In Controller.SelectedPaths
                ExportToPar(Controller.ApsimData.Find(SelectedPath), Dialog.FileName, Controller)
            Next
            MessageBox.Show("Soils have been exported to '" + Dialog.FileName + "'", "Success", MessageBoxButtons.OK, MessageBoxIcon.Information)
        End If
    End Sub
    Private Shared Sub ExportToPar(ByVal Data As ApsimFile.Component, ByVal FileName As String, ByVal Controller As BaseController)
        If Data.Type.ToLower() = "folder" Then
            For Each Child As ApsimFile.Component In Data.ChildNodes
                ExportToPar(Child, FileName, Controller)
            Next
        Else
            Dim SoilToExport As Soil = Soil.CreateFromXML(Data.FullXML())
            'SoilToExport.ExportToPar(FileName, SoilToExport.Name, True)
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
            Dim ThisSoil As Soil = Soil.CreateFromXML(Data.Contents)
            Dim Errors As String = ThisSoil.CheckForErrors()
            If Errors <> "" Then
                ErrorMessage += vbCr + vbLf + ThisSoil.Name + vbCr + vbLf + StringManip.IndentText(Errors, 6)
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
    Public Shared Sub CheckWebForDataUpdate(ByVal Controller As BaseController)
        Cursor.Current = Cursors.WaitCursor
        Dim request As WebRequest = WebRequest.Create("http://www.apsim.info/apsim/downloads/APSRU-Australia-Soils.soils")
        Dim response As HttpWebResponse = DirectCast(request.GetResponse(), HttpWebResponse)
        If response.StatusDescription = "OK" Then
            Dim dataStream As Stream = response.GetResponseStream()
            Dim reader As New StreamReader(dataStream)
            Dim responseFromServer As String = reader.ReadToEnd()

            Dim SoilsFileName As String = Configuration.Instance.Setting("FileNameToUpdateFromWeb")
            Dim SoilsFile As New StreamWriter(SoilsFileName)
            SoilsFile.Write(responseFromServer)
            SoilsFile.Close()
            reader.Close()
            dataStream.Close()
            MessageBox.Show("The file: " + SoilsFileName + " has been updated to the latest version.", "Success", MessageBoxButtons.OK, MessageBoxIcon.Information)
        Else
            MessageBox.Show("Cannot connection to www.apsim.info", "Failure", MessageBoxButtons.OK, MessageBoxIcon.[Error])
        End If
        response.Close()
        Cursor.Current = Cursors.[Default]
    End Sub
    Public Shared Sub ReleaseNotes(ByVal Controller As BaseController)
        Dim URL As String = Configuration.Instance.Setting("ReleaseNotes")
        System.Diagnostics.Process.Start(URL)
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


