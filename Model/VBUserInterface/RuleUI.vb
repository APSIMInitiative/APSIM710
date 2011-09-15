
Imports System.Collections
Imports System.Collections.Specialized
Imports System.Collections.Generic
Imports System.IO
Imports System.Xml

Imports Controllers
Imports CSGeneral
Imports UIBits      'InputDialog

'nb. this overloads Generic UI. This RuleUI is just GenericUI with a Script tab added on, so the user can also write their own management scripts.

Public Class RuleUI
    Inherits BaseView
    Private InRefresh As Boolean
    Friend WithEvents GenericUI As GenericUI
    Friend WithEvents PopupMenu As System.Windows.Forms.ContextMenuStrip
    Friend WithEvents AddMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents DeleteMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripSeparator1 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents EditMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents PropertiesMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ImageList As System.Windows.Forms.ImageList
    Private Cultivars As XmlNode


#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        'Add any initialization after the InitializeComponent() call

    End Sub

    'Form overrides dispose to clean up the component list.
    Protected Overloads Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing Then
            If Not (components Is Nothing) Then
                components.Dispose()
            End If
        End If
        MyBase.Dispose(disposing)
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    Friend WithEvents TabControl As System.Windows.Forms.TabControl
    Friend WithEvents PropertiesTabPage As System.Windows.Forms.TabPage
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(RuleUI))
        Me.TabControl = New System.Windows.Forms.TabControl
        Me.PopupMenu = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.AddMenuItem = New System.Windows.Forms.ToolStripMenuItem
        Me.DeleteMenuItem = New System.Windows.Forms.ToolStripMenuItem
        Me.EditMenuItem = New System.Windows.Forms.ToolStripMenuItem
        Me.ToolStripSeparator1 = New System.Windows.Forms.ToolStripSeparator
        Me.PropertiesMenuItem = New System.Windows.Forms.ToolStripMenuItem
        Me.PropertiesTabPage = New System.Windows.Forms.TabPage
        Me.GenericUI = New VBUserInterface.GenericUI
        Me.ImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.TabControl.SuspendLayout()
        Me.PopupMenu.SuspendLayout()
        Me.PropertiesTabPage.SuspendLayout()
        Me.SuspendLayout()
        '
        'MyHelpLabel
        '
        Me.MyHelpLabel.Size = New System.Drawing.Size(1022, 16)
        '
        'TabControl
        '
        Me.TabControl.ContextMenuStrip = Me.PopupMenu
        Me.TabControl.Controls.Add(Me.PropertiesTabPage)
        Me.TabControl.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TabControl.Location = New System.Drawing.Point(0, 16)
        Me.TabControl.Name = "TabControl"
        Me.TabControl.SelectedIndex = 0
        Me.TabControl.Size = New System.Drawing.Size(1022, 800)
        Me.TabControl.TabIndex = 3
        '
        'PopupMenu
        '
        Me.PopupMenu.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.AddMenuItem, Me.DeleteMenuItem, Me.EditMenuItem, Me.ToolStripSeparator1, Me.PropertiesMenuItem})
        Me.PopupMenu.Name = "ContextMenuStrip"
        Me.PopupMenu.Size = New System.Drawing.Size(236, 120)
        '
        'AddMenuItem
        '
        Me.AddMenuItem.Name = "AddMenuItem"
        Me.AddMenuItem.Size = New System.Drawing.Size(235, 22)
        Me.AddMenuItem.Text = "&Add another script item"
        '
        'DeleteMenuItem
        '
        Me.DeleteMenuItem.Name = "DeleteMenuItem"
        Me.DeleteMenuItem.Size = New System.Drawing.Size(235, 22)
        Me.DeleteMenuItem.Text = "&Delete this script item"
        '
        'EditMenuItem
        '
        Me.EditMenuItem.Name = "EditMenuItem"
        Me.EditMenuItem.Size = New System.Drawing.Size(235, 22)
        Me.EditMenuItem.Text = "&Edit this script item"
        '
        'ToolStripSeparator1
        '
        Me.ToolStripSeparator1.Name = "ToolStripSeparator1"
        Me.ToolStripSeparator1.Size = New System.Drawing.Size(232, 6)
        '
        'PropertiesMenuItem
        '
        Me.PropertiesMenuItem.Name = "PropertiesMenuItem"
        Me.PropertiesMenuItem.Size = New System.Drawing.Size(235, 22)
        Me.PropertiesMenuItem.Text = "Add a &properties user interface"
        '
        'PropertiesTabPage
        '
        Me.PropertiesTabPage.Controls.Add(Me.GenericUI)
        Me.PropertiesTabPage.Location = New System.Drawing.Point(4, 22)
        Me.PropertiesTabPage.Name = "PropertiesTabPage"
        Me.PropertiesTabPage.Size = New System.Drawing.Size(1014, 774)
        Me.PropertiesTabPage.TabIndex = 0
        Me.PropertiesTabPage.Text = "Properties"
        Me.PropertiesTabPage.UseVisualStyleBackColor = True
        '
        'GenericUI
        '
        Me.GenericUI.AutoScroll = True
        Me.GenericUI.BackColor = System.Drawing.SystemColors.Control
        Me.GenericUI.Dock = System.Windows.Forms.DockStyle.Fill
        Me.GenericUI.HelpText = ""
        Me.GenericUI.Location = New System.Drawing.Point(0, 0)
        Me.GenericUI.Name = "GenericUI"
        Me.GenericUI.Size = New System.Drawing.Size(1014, 774)
        Me.GenericUI.TabIndex = 0
        '
        'ImageList
        '
        Me.ImageList.ImageStream = CType(resources.GetObject("ImageList.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.ImageList.TransparentColor = System.Drawing.Color.Transparent
        Me.ImageList.Images.SetKeyName(0, "find.png")
        '
        'RuleUI
        '
        Me.Controls.Add(Me.TabControl)
        Me.Name = "RuleUI"
        Me.Size = New System.Drawing.Size(1022, 816)
        Me.Controls.SetChildIndex(Me.MyHelpLabel, 0)
        Me.Controls.SetChildIndex(Me.TabControl, 0)
        Me.TabControl.ResumeLayout(False)
        Me.PopupMenu.ResumeLayout(False)
        Me.PropertiesTabPage.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

#End Region

    Protected Overrides Sub OnLoad()
    End Sub
    ' -----------------------------------
    ' Refresh the UI
    ' -----------------------------------
    Overrides Sub OnRefresh()

        InRefresh = True

        TabControl.TabPages.Clear()

        ' Fill the property grid.
        Dim UINode As XmlNode = XmlHelper.Find(Data, "ui")
        If Not IsNothing(UINode) Then
            TabControl.TabPages.Add(PropertiesTabPage)
            GenericUI.OnLoad(Controller, NodePath, UINode.OuterXml)
            GenericUI.OnRefresh()
        End If

        ' Create tabs for each script tag.
        For Each Script As XmlNode In XmlHelper.ChildNodes(Data, "script")
            Dim TabName As String = ""
            For Each EventData As XmlNode In XmlHelper.ChildNodes(Script, "event")
                If TabName <> "" Then
                    TabName = TabName + ","
                End If
                TabName = TabName + EventData.InnerText
            Next
            Dim Value As String = XmlHelper.Value(Script, "text")
            AddScriptTab(TabName, Value)
        Next

        InRefresh = False
    End Sub

    Private Sub AddScriptTab(ByVal TabName As String, ByVal Value As String)
        Dim page As New TabPage(TabName)

        'Add a menu to the page at the top.

        Dim ToolStrip As New ToolStrip()
        ToolStrip.Parent = page
        ToolStrip.ImageList = ImageList
        ToolStrip.Dock = DockStyle.Top
        AddHandler ToolStrip.ItemClicked, AddressOf OnItemClicked
        Dim FindReplaceButton As ToolStripItem = ToolStrip.Items.Add("Find/Replace")
        FindReplaceButton.ImageIndex = 0

        Dim ScriptBox As New QWhale.Editor.SyntaxEdit
        ScriptBox.Text = Value
        ScriptBox.WordWrap = False
        ScriptBox.Gutter.Options = CType((((QWhale.Editor.GutterOptions.PaintLineNumbers Or QWhale.Editor.GutterOptions.PaintLinesOnGutter) _
                            Or QWhale.Editor.GutterOptions.PaintBookMarks) _
                            Or QWhale.Editor.GutterOptions.PaintLineModificators), QWhale.Editor.GutterOptions)

        page.Controls.Add(ScriptBox)
        ScriptBox.Dock = DockStyle.Fill
        ScriptBox.BringToFront()
        Dim TabStops() As Integer = {3}
        ScriptBox.Lines.TabStops = TabStops
        ScriptBox.Lines.UseSpaces = True
        TabControl.TabPages.Add(page)
    End Sub

    Public Overrides Sub OnSave()
        ' --------------------------------------
        ' Save the script box if it has changd.
        ' --------------------------------------
        Dim Contents As String = ""
        If TabControl.TabPages(0).Text = "Properties" Then
            GenericUI.OnSave()
            Contents = GenericUI.GetData()
        End If
        Data.RemoveAll()

        If Contents <> "" Then
            Dim Doc As New XmlDocument
            Doc.LoadXml(Contents)
            Data.AppendChild(Data.OwnerDocument.ImportNode(Doc.DocumentElement, True))
        End If

        For Each Page As TabPage In TabControl.TabPages
            If Page.Text <> "Properties" Then
                Dim Script As XmlNode = Data.AppendChild(Data.OwnerDocument.CreateElement("script"))
            Dim ScriptBox As QWhale.Editor.SyntaxEdit = Page.Controls.Item(0)
                XmlHelper.SetValue(Script, "text", ScriptBox.Text)

                Dim EventNames As String() = Page.Text.Split(",".ToCharArray())
                Dim Events As New List(Of String)
                Events.AddRange(EventNames)
                XmlHelper.SetValues(Script, "event", Events)
            End If
        Next
    End Sub

    Private Sub OnAddMenuClick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles AddMenuItem.Click
        Dim EventNamesString As String = UIBits.InputDialog.InputBox("Enter event name(s) to run script on", "APSIM event names (comma separated)", "", False)
        If EventNamesString <> "" Then
            AddScriptTab(EventNamesString, "")
            TabControl.SelectedIndex = TabControl.TabCount - 1
        End If
    End Sub

    Private Sub OnDeleteMenuClick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles DeleteMenuItem.Click
        Dim CurrentTabName As String = TabControl.TabPages(TabControl.SelectedIndex).Text
        If MessageBox.Show("Are you sure you want to delete " + CurrentTabName + "?", "Confirmation required", MessageBoxButtons.YesNo, MessageBoxIcon.Question) = DialogResult.Yes Then
            TabControl.TabPages.Remove(TabControl.SelectedTab)
        End If
    End Sub

    Private Sub OnEditMenuClick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles EditMenuItem.Click
        Dim EventNamesString As String = UIBits.InputDialog.InputBox("Enter event name(s) to run script on", "APSIM event names (comma separated)", TabControl.SelectedTab.Text, False)
        If EventNamesString <> TabControl.SelectedTab.Text Then
            TabControl.SelectedTab.Text = EventNamesString
        End If
    End Sub

    Private Sub OnPropertiesMenuClick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PropertiesMenuItem.Click
        TabControl.TabPages.Insert(0, PropertiesTabPage)
        Dim UINode As XmlNode = XmlHelper.Find(Data, "ui")
        If IsNothing(UINode) Then
            UINode = Data.AppendChild(Data.OwnerDocument.CreateElement("ui"))
        End If

        GenericUI.OnLoad(Controller, NodePath, UINode.OuterXml)
        GenericUI.OnRefresh()
    End Sub

    Private Sub OnPopupOpening(ByVal sender As System.Object, ByVal e As System.ComponentModel.CancelEventArgs) Handles PopupMenu.Opening
        DeleteMenuItem.Enabled = TabControl.TabPages.Count > 1
        EditMenuItem.Enabled = TabControl.SelectedTab.Text <> "Properties"
        PropertiesMenuItem.Enabled = TabControl.TabPages(0).Text <> "Properties"
    End Sub

    Private Sub OnItemClicked(ByVal sender As Object, ByVal E As ToolStripItemClickedEventArgs)
        Dim ScriptBox As QWhale.Editor.SyntaxEdit = TabControl.SelectedTab.Controls.Item(0)
        ScriptBox.DisplaySearchDialog()
    End Sub
End Class
