
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
                ParFileImporter.ImportParFile(FileName, Controller)
            Next
        End If
    End Sub
    Public Shared Sub ImportFromSpreadsheet(ByVal Controller As BaseController)
        Dim Dialog As New OpenFileDialog()
        Dialog.Filter = "Spreadsheet files (*.xls)|*.xls|All files (*.*)|*.*"
        Dialog.Title = "Select a spreadsheet to import from"
        Dialog.RestoreDirectory = True
        If Dialog.ShowDialog() = DialogResult.OK Then
            SoilSpreadsheet.ImportFromFile(Dialog.FileName, Controller)
        End If
    End Sub

    Public Shared Sub ImportFromW2N2(ByVal Controller As BaseController)
        Dim Dialog As New FolderBrowserDialog()
        If Dialog.ShowDialog() = DialogResult.OK Then
            Dim Files As String() = Directory.GetFiles(Dialog.SelectedPath, "*.w2")
            ParFileImporter.ImportW2N2P2(Files, Controller)
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
            Dim Doc As New XmlDocument()
            Doc.LoadXml(Data.FullXML())
            Dim SoilToExport As New Soil(Doc.DocumentElement)
            SoilToExport.ExportToPar(FileName, SoilToExport.Name, True)
        End If
    End Sub
    Public Shared Sub ExportToSpreadsheet(ByVal Controller As BaseController)
        Dim Dialog As New SaveFileDialog()
        Dialog.Filter = "Spreadsheet files (*.xls)|*.xls|All files (*.*)|*.*"
        Dialog.Title = "Enter a spreadsheet file to export to"
        Dialog.DefaultExt = "xls"
        If Dialog.ShowDialog() = DialogResult.OK Then
            SoilSpreadsheet.ExportSelectedToFile(Dialog.FileName, Controller)
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
            Dim Doc As New XmlDocument()
            Doc.LoadXml(Data.Contents)

            Dim ThisSoil As New Soil(Doc.DocumentElement)
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
    Public Shared Sub SoilCropManagement(ByVal Controller As BaseController)
        Dim Doc As New XmlDocument()
        Doc.LoadXml(Controller.Selection.Contents)
        Dim MySoil As New Soil(Doc.DocumentElement)

        Dim Form As New UIBits.ReorderForm()
        Form.Text = "Soil / Crop Management"
        Form.TextForAddPrompt = "Enter the name of a new crop"
        Form.SetItems(MySoil.CropsMeasured)
        If Form.ShowDialog() = DialogResult.OK Then
            For Each CropName As String In Form.GetItems()
                If Not MySoil.CropExists(CropName) Then
                    MySoil.AddCrop(CropName)
                End If
            Next
            Dim Crops As String() = MySoil.CropsMeasured
            For Each CropName As String In Crops
                If CSGeneral.StringManip.IndexOfCaseInsensitive(Form.GetItems(), CropName) = -1 Then
                    MySoil.DeleteCrop(CropName)
                End If
            Next
            MySoil.SetCropOrder(Form.GetItems())
            Controller.Selection.Contents = Doc.DocumentElement.OuterXml
            Controller.Explorer.RefreshCurrentView()
        End If
    End Sub
    Public Shared Sub ChangePHUnits(ByVal Controller As BaseController)
        Dim Doc As New XmlDocument()
        Doc.LoadXml(Controller.Selection.Contents)
        Dim SelectedData As XmlNode = Doc.DocumentElement
        Dim MySoil As New Soil(SelectedData)
        If MySoil.PHStoredAsWater() Then
            MySoil.PHCaCl = MySoil.PH
        Else
            MySoil.PH = MySoil.PHCaCl
        End If
        Controller.Selection.Contents = SelectedData.OuterXml
        Controller.Explorer.RefreshCurrentView()
    End Sub
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
        Dim SoilUI As CSUserInterface.SoilUI = DirectCast(Controller.Explorer.CurrentView, CSUserInterface.SoilUI)

        Dim PrintDocument As New System.Drawing.Printing.PrintDocument()
        AddHandler PrintDocument.BeginPrint, AddressOf SoilUI.OnBeginPrint
        AddHandler PrintDocument.PrintPage, AddressOf SoilUI.OnPrintPage
        AddHandler PrintDocument.QueryPageSettings, AddressOf OnQueryPageSettings

        Dim PrintDialog As New PrintDialog()
        PrintDialog.Document = PrintDocument

        Dim PreviewDialog As New System.Windows.Forms.PrintPreviewDialog()
        PreviewDialog.Document = PrintDocument
        PreviewDialog.ShowDialog()
    End Sub
    Private Shared Sub OnQueryPageSettings(ByVal sender As Object, ByVal e As System.Drawing.Printing.QueryPageSettingsEventArgs)
        e.PageSettings.Margins.Left = 50
        e.PageSettings.Margins.Top = 50
        e.PageSettings.Margins.Right = 50
        e.PageSettings.Margins.Bottom = 50
    End Sub
#End Region



#Region "SIM file writing stuff"
    Public Shared Function WriteSoilSim(ByVal Component As ApsimFile.Component, ByVal ParentNode As XmlNode) As XmlNode
        Dim Doc As New XmlDocument()
        Doc.LoadXml(Component.Contents)
        Dim Soil As New ApsimFile.Soil(Doc.DocumentElement)
        Return Soil.ExportToSim(ParentNode)
    End Function
    Public Shared Function WriteCropSim(ByVal Component As ApsimFile.Component, ByVal ParentNode As XmlNode) As XmlNode
        ' Go find our related soil - must be a sibling. 
        Dim SoilComponent As ApsimFile.Component = Nothing
        Dim Paddock As ApsimFile.Component = Component.FindContainingPaddock()
        If (Paddock Is Nothing) Then
            Throw New Exception("Cannot find containing paddock for component: " + Component.Name)
        End If
        For Each Sibling As ApsimFile.Component In Paddock.ChildNodes
         If Sibling.Type.ToLower() = "soil" Then
            SoilComponent = Sibling
         End If
        Next

        If (SoilComponent Is Nothing) Then
            Throw New Exception("Cannot find a soil component")
        End If
        Dim Doc As New XmlDocument()
        Doc.LoadXml(SoilComponent.Contents)
        Dim Soil As New ApsimFile.Soil(Doc.DocumentElement)
        Return Soil.ExportCropToSim(ParentNode, Component.Type)
    End Function
    Public Shared Function WriteInitWaterSim(ByVal Component As ApsimFile.Component, ByVal ParentNode As XmlNode) As XmlNode
        ' Go find our related soil - should be parent 

        Dim SoilComponent As ApsimFile.Component = Component.Parent
        If (SoilComponent Is Nothing) Then
            Throw New Exception("Cannot find a soil component")
        End If
        Dim Doc As New XmlDocument()
        Doc.LoadXml(SoilComponent.Contents)
        Dim Soil As New ApsimFile.Soil(Doc.DocumentElement)

        ' Go create an initwater object. 
        Dim InitWaterDoc As New XmlDocument()
        InitWaterDoc.LoadXml(Component.Contents)
        Dim InitWater As New ApsimFile.InitWater(InitWaterDoc.DocumentElement, Soil)
        Return InitWater.ExportToSim(ParentNode)

    End Function
    Public Shared Function WriteSoilSampleSim(ByVal Component As ApsimFile.Component, ByVal ParentNode As XmlNode) As XmlNode
        ' Go find our related soil - should be parent 

        Dim SoilComponent As ApsimFile.Component = Component.Parent
        If (SoilComponent Is Nothing) Then
            Throw New Exception("Cannot find a soil component")
        End If
        Dim Doc As New XmlDocument()
        Doc.LoadXml(SoilComponent.Contents)
        Dim Soil As New ApsimFile.Soil(Doc.DocumentElement)

        ' Go create an initwater object. 
        Dim SampleDoc As New XmlDocument()
        SampleDoc.LoadXml(Component.Contents)
        Dim Sample As New ApsimFile.SoilSample(SampleDoc.DocumentElement, Soil)
        Return Sample.ExportToSim(ParentNode)

    End Function
    Public Shared Function WriteInitNitrogenSim(ByVal Component As ApsimFile.Component, ByVal ParentNode As XmlNode) As XmlNode
        ' Go find our related soil - should be parent 
        Dim SoilComponent As ApsimFile.Component = Component.Parent
        If (SoilComponent Is Nothing) Then
            Throw New Exception("Cannot find a soil component")
        End If
        Dim Doc As New XmlDocument()
        Doc.LoadXml(SoilComponent.Contents)
        Dim Soil As New ApsimFile.Soil(Doc.DocumentElement)

        ' Find the <component name="nitrogen"> node 
        Dim NitrogenComponentName As String = XmlHelper.Name(ParentNode.ParentNode).Replace(" Water", " Nitrogen")
        Dim NitrogenSimNode As XmlNode = XmlHelper.Find(ParentNode.ParentNode.ParentNode, NitrogenComponentName)
        If (NitrogenSimNode Is Nothing) Then
            Throw New Exception("Cannot find soiln2 node")
        End If

        ' Go create an initwater object. 
        Dim InitNitrogenDoc As New XmlDocument()
        InitNitrogenDoc.LoadXml(Component.Contents)
        Dim InitNitrogen As New ApsimFile.InitNitrogen(InitNitrogenDoc.DocumentElement, Soil)
        Return InitNitrogen.ExportToSim(NitrogenSimNode)

    End Function
    Public Shared Function WriteManagerSim(ByVal Component As ApsimFile.Component, ByVal ParentNode As XmlNode) As XmlNode
        Dim Doc As New XmlDocument()
        Doc.LoadXml(Component.Contents)

        For Each Script As XmlNode In XmlHelper.ChildNodes(Doc.DocumentElement, "")
            Dim Contents As String = XmlHelper.Value(Script, "text")
            Dim Events As List(Of String) = XmlHelper.Values(Script, "event")

            ' Do manager macro replacement. 
            Dim UI As XmlNode = XmlHelper.Find(Doc.DocumentElement, "ui")
            Contents = Contents.Replace("[Name]", Component.Name)
            Contents = Contents.Replace("[name]", Component.Name)
            For Each Prop As XmlNode In XmlHelper.ChildNodes(UI, "")
            If Prop.Name.ToLower() <> "category" Then
                    Dim MacroToLookFor As String = "[" + Prop.Name + "]"
               Contents = Contents.Replace(MacroToLookFor, Prop.InnerText)
               Dim i As Integer = 0
               While i <> Events.Count
                  Events(i) = Events(i).Replace(MacroToLookFor, Prop.InnerText)
                  i += 1
               End While
            End If
         Next

            ' For each event, write a rule node to parent node. 
         For Each Evnt As String In Events
                Dim RuleName As String = XmlHelper.Name(Script) + " - " + Evnt

            Dim RuleNode As XmlNode = ParentNode.AppendChild(ParentNode.OwnerDocument.CreateElement("rule"))
            XmlHelper.SetName(RuleNode, RuleName)
            XmlHelper.SetAttribute(RuleNode, "condition", Evnt)
            XmlHelper.SetValue(RuleNode, "", Contents)
         Next
        Next
        Return ParentNode
    End Function

#End Region



End Class


