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
        If Not IsNothing(UINode) Then
            GenericUI.OnLoad(Controller, NodePath, UINode.OuterXml)
            GenericUI.OnRefresh()
        End If
        TextBox.Text = XmlHelper.Value(Data, "text")
        If TextBox.Text.Contains("Imports ") Then
            TextBox.Lexer = VbParser
        Else
            TextBox.Lexer = CsParser
        End If
        Assembly.LoadFile(Path.GetDirectoryName(Application.ExecutablePath) + "\" + "DotNetComponentInterface.dll")
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
        GenericUI.OnSave()
        Dim Contents As String = GenericUI.GetData()

        Data.RemoveAll()

        If Contents <> "" Then
            Dim Doc As New XmlDocument
            Doc.LoadXml(Contents)
            Data.AppendChild(Data.OwnerDocument.ImportNode(Doc.DocumentElement, True))
        End If

        XmlHelper.SetValue(Data, "text", TextBox.Text)
    End Sub


End Class
