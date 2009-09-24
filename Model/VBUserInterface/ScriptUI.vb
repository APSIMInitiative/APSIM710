Imports System.Xml
Imports CSGeneral
Imports System.IO
Imports System.Reflection
Public Class ScriptUI

    ' -----------------------------------
    ' Refresh the UI
    ' -----------------------------------
    Overrides Sub OnRefresh()
        ' Fill the property grid.
        Dim UINode As XmlNode = XmlHelper.Find(Data, "ui")
        If IsNothing(UINode) Then
            TabControl.TabPages.Remove(Properties)

        Else
            GenericUI.OnLoad(Controller, NodePath, UINode.OuterXml)
            GenericUI.OnRefresh()
        End If
        PropertiesMenuItem.Checked = TabControl.TabPages.Count = 2
        TextBox.Text = XmlHelper.Value(Data, "text")
        If TextBox.Text.Contains("Imports ") Then
            TextBox.Lexer = VbParser
        Else
            TextBox.Lexer = CsParser
        End If
        Assembly.LoadFile(Path.GetDirectoryName(Application.ExecutablePath) + "\" + "DotNetComponentInterface.dll")
        Dim ManagerHelpers As List(Of String) = LoadManagerHelpers.GetManagerHelpers()
        For Each ManagerHelper As String In ManagerHelpers
            Assembly.LoadFile(ManagerHelper)
        Next
        CsParser.RegisterAllAssemblies()
        VbParser.RegisterAllAssemblies()
        Dim TabStops() As Integer = {3}
        TextBox.Lines.TabStops = TabStops
        TextBox.Lines.UseSpaces = True
    End Sub

    Public Overrides Sub OnSave()
        ' --------------------------------------
        ' Save the script box if it has changd.
        ' --------------------------------------
        Dim Contents As String = ""
        If Not IsNothing(GenericUI) Then
            GenericUI.OnSave()
            Contents = GenericUI.GetData()
        End If

        Data.RemoveAll()

        If Contents <> "" And PropertiesMenuItem.Checked Then
            Dim Doc As New XmlDocument
            Doc.LoadXml(Contents)
            Data.AppendChild(Data.OwnerDocument.ImportNode(Doc.DocumentElement, True))
        End If

        XmlHelper.SetValue(Data, "text", TextBox.Text)
    End Sub


    Private Sub OnButtonClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.ToolStripItemClickedEventArgs) Handles ToolStrip1.ItemClicked
        TextBox.DisplaySearchDialog()
    End Sub

    Private Sub OnPropertiesMenuItemClick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PropertiesMenuItem.Click
        If TabControl.TabPages.Count = 1 Then
            TabControl.TabPages.Insert(0, Properties)
        Else
            TabControl.TabPages.Remove(Properties)
        End If
        PropertiesMenuItem.Checked = TabControl.TabPages.Count = 2
    End Sub
End Class
