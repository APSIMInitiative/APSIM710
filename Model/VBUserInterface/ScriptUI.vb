Imports System.Xml
Imports CSGeneral
Imports System.IO
Imports System.Reflection
Imports System.Runtime.InteropServices
Public Class ScriptUI

    ' -----------------------------------
    ' Refresh the UI
    ' -----------------------------------
    Overrides Sub OnRefresh()
        ' Fill the property grid.
        Dim UINode As XmlNode = XmlHelper.Find(Data, "ui")
        Dim UIXml As String = "<ui/>"
        If IsNothing(UINode) Then
            TabControl.TabPages.Remove(Properties)

        Else
            If TabControl.TabPages.Count = 1 Then
                TabControl.TabPages.Insert(0, Properties)
            End If
            UIXml = UINode.OuterXml
        End If
        GenericUI.OnLoad(Controller, NodePath, UIXml)
        GenericUI.OnRefresh()

        PropertiesMenuItem.Checked = TabControl.TabPages.Count = 2

        TextBox.Text = XmlHelper.Value(Data, "text")
        If TextBox.Text.Contains("Imports ") Then
            TextBox.Lexer = VbParser
        Else
            TextBox.Lexer = CsParser
        End If
        Assembly.LoadFile(Path.GetDirectoryName(Application.ExecutablePath) + "\" + "CSDotNetComponentInterface.dll")
        Assembly.LoadFile(Types.GetProbeInfoDLLFileName())

        For Each ref As String In XmlHelper.ValuesRecursive(Data, "reference")
            If File.Exists(ref) Then
                Assembly.LoadFile(ref)
            ElseIf File.Exists(RuntimeEnvironment.GetRuntimeDirectory() + ref) Then
                Assembly.LoadFile(RuntimeEnvironment.GetRuntimeDirectory() + ref)
            ElseIf File.Exists(Path.GetDirectoryName(Application.ExecutablePath) + "\" + ref) Then
                Assembly.LoadFile(Path.GetDirectoryName(Application.ExecutablePath) + "\" + ref)
            Else
            	MessageBox.Show("Error loading reference '" + ref + "' - file does not exist" + Environment.NewLine +
            	"Tried:" + Environment.NewLine +
            	ref + Environment.NewLine +
            	RuntimeEnvironment.GetRuntimeDirectory() + ref + Environment.NewLine +
            	Path.GetDirectoryName(Application.ExecutablePath) + "\" + ref
            	)
            End If
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

        Dim refs As List(Of XmlNode) = New List(Of XmlNode)()
        refs = XmlHelper.ChildNodes(Data, "reference")

        Data.RemoveAll()

        If Contents <> "" And PropertiesMenuItem.Checked Then
            Dim Doc As New XmlDocument
            Doc.LoadXml(Contents)
            Data.AppendChild(Data.OwnerDocument.ImportNode(Doc.DocumentElement, True))
        End If

        For Each xn As XmlNode In refs
            Data.AppendChild(Data.OwnerDocument.ImportNode(xn, True))
        Next

        XmlHelper.SetValue(Data, "text", TextBox.Text)

    End Sub


    Private Sub OnButtonClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.ToolStripItemClickedEventArgs) Handles ToolStrip1.ItemClicked
        TextBox.DisplaySearchDialog()
    End Sub

    Private Sub OnPropertiesMenuItemClick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PropertiesMenuItem.Click
        If TabControl.TabPages.Count = 1 Then
            TabControl.TabPages.Insert(0, Properties)
            GenericUI.OnLoad(Controller, NodePath, "<ui/>")
            GenericUI.OnRefresh()
        Else
            TabControl.TabPages.Remove(Properties)
        End If
        PropertiesMenuItem.Checked = TabControl.TabPages.Count = 2
    End Sub

End Class


