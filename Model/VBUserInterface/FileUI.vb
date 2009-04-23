
Imports System
Imports System.IO

Imports ApsimFile
Imports Controllers
Imports CSGeneral


Public Class FileUI
    Inherits BaseView

    Private FileDateTime As DateTime
    Friend WithEvents BrowseButton As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripLabel1 As System.Windows.Forms.ToolStripLabel
    Friend WithEvents SearchTextBox As System.Windows.Forms.ToolStripTextBox
    Friend WithEvents SearchButton As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStrip1 As System.Windows.Forms.ToolStrip
    Dim FullFileName As String

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
    Friend WithEvents OpenFileDialog As System.Windows.Forms.OpenFileDialog
    Friend WithEvents ImageList As System.Windows.Forms.ImageList
    Friend WithEvents FileContentsBox As System.Windows.Forms.RichTextBox
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FileUI))
        Me.ImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.FileContentsBox = New System.Windows.Forms.RichTextBox
        Me.OpenFileDialog = New System.Windows.Forms.OpenFileDialog
        Me.BrowseButton = New System.Windows.Forms.ToolStripButton
        Me.ToolStripLabel1 = New System.Windows.Forms.ToolStripLabel
        Me.SearchTextBox = New System.Windows.Forms.ToolStripTextBox
        Me.SearchButton = New System.Windows.Forms.ToolStripButton
        Me.ToolStrip1 = New System.Windows.Forms.ToolStrip
        Me.ToolStrip1.SuspendLayout()
        Me.SuspendLayout()
        '
        'MyHelpLabel
        '
        Me.MyHelpLabel.Size = New System.Drawing.Size(794, 16)
        '
        'ImageList
        '
        Me.ImageList.ImageStream = CType(resources.GetObject("ImageList.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.ImageList.TransparentColor = System.Drawing.Color.Transparent
        Me.ImageList.Images.SetKeyName(0, "")
        Me.ImageList.Images.SetKeyName(1, "")
        Me.ImageList.Images.SetKeyName(2, "")
        Me.ImageList.Images.SetKeyName(3, "")
        '
        'FileContentsBox
        '
        Me.FileContentsBox.Dock = System.Windows.Forms.DockStyle.Fill
        Me.FileContentsBox.Font = New System.Drawing.Font("Courier New", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.FileContentsBox.Location = New System.Drawing.Point(0, 41)
        Me.FileContentsBox.Name = "FileContentsBox"
        Me.FileContentsBox.ReadOnly = True
        Me.FileContentsBox.Size = New System.Drawing.Size(794, 427)
        Me.FileContentsBox.TabIndex = 3
        Me.FileContentsBox.Text = ""
        Me.FileContentsBox.WordWrap = False
        '
        'OpenFileDialog
        '
        Me.OpenFileDialog.Filter = "All files|*.*"
        Me.OpenFileDialog.RestoreDirectory = True
        '
        'BrowseButton
        '
        Me.BrowseButton.Image = CType(resources.GetObject("BrowseButton.Image"), System.Drawing.Image)
        Me.BrowseButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.BrowseButton.Name = "BrowseButton"
        Me.BrowseButton.Size = New System.Drawing.Size(62, 22)
        Me.BrowseButton.Text = "Browse"
        Me.BrowseButton.ToolTipText = "Browse for files"
        '
        'ToolStripLabel1
        '
        Me.ToolStripLabel1.Name = "ToolStripLabel1"
        Me.ToolStripLabel1.Size = New System.Drawing.Size(44, 22)
        Me.ToolStripLabel1.Text = "Search:"
        '
        'SearchTextBox
        '
        Me.SearchTextBox.Name = "SearchTextBox"
        Me.SearchTextBox.Size = New System.Drawing.Size(100, 25)
        '
        'SearchButton
        '
        Me.SearchButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.SearchButton.Image = CType(resources.GetObject("SearchButton.Image"), System.Drawing.Image)
        Me.SearchButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.SearchButton.Name = "SearchButton"
        Me.SearchButton.Size = New System.Drawing.Size(23, 22)
        Me.SearchButton.Text = "ToolStripButton1"
        Me.SearchButton.ToolTipText = "Search the file for this text"
        '
        'ToolStrip1
        '
        Me.ToolStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.BrowseButton, Me.ToolStripLabel1, Me.SearchTextBox, Me.SearchButton})
        Me.ToolStrip1.Location = New System.Drawing.Point(0, 16)
        Me.ToolStrip1.Name = "ToolStrip1"
        Me.ToolStrip1.Size = New System.Drawing.Size(794, 25)
        Me.ToolStrip1.TabIndex = 16
        Me.ToolStrip1.Text = "ToolStrip1"
        '
        'FileUI
        '
        Me.Controls.Add(Me.FileContentsBox)
        Me.Controls.Add(Me.ToolStrip1)
        Me.Name = "FileUI"
        Me.Size = New System.Drawing.Size(794, 468)
        Me.Controls.SetChildIndex(Me.MyHelpLabel, 0)
        Me.Controls.SetChildIndex(Me.ToolStrip1, 0)
        Me.Controls.SetChildIndex(Me.FileContentsBox, 0)
        Me.ToolStrip1.ResumeLayout(False)
        Me.ToolStrip1.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

#End Region

    Overrides Sub OnRefresh()
        ' ------------------------------
        ' Refresh this window.
        ' ------------------------------

        ' Get a filename
        Dim FileName As String
        If XmlHelper.Type(Data) = "outputfile" Or XmlHelper.Type(Data) = "summaryfile" Then
            FileName = ApsimFile.ComponentUtility.CalcFileName(Controller.ApsimData.Find(NodePath))
            FileContentsBox.ReadOnly = True
        Else
            FileName = XmlHelper.Value(Data, "filename")
            FileContentsBox.ReadOnly = False
        End If

        HelpText = FileName

        FullFileName = Configuration.RemoveMacros(FileName)
        ' Add a path to filename if necessary.
        If Controller.ApsimData.FileName <> "" Then
            FullFileName = Path.Combine(Path.GetDirectoryName(Controller.ApsimData.FileName), FullFileName)
        Else
            FullFileName = FullFileName
        End If

        If File.Exists(FullFileName) Then
            Dim text As String
            Dim sr As StreamReader

            Try
                sr = New StreamReader(FullFileName)
                text = sr.ReadToEnd
                sr.Close()
                sr = Nothing
                FileContentsBox.Text = text
                FileDateTime = File.GetLastWriteTime(FullFileName)
            Catch e As System.Exception
            End Try

        Else
            FileContentsBox.Text = "<File doesn't exist>"
        End If
        Dim C As Control = Parent
        While Not IsNothing(C)
            If C.Name = "MainUI" Then
                Dim F As Form = C
                AddHandler F.Activated, AddressOf OnActivate
                Exit While
            Else
                C = C.Parent
            End If
        End While

        BrowseButton.Visible = (XmlHelper.Type(Data) <> "outputfile" And XmlHelper.Type(Data) <> "summaryfile")
    End Sub


    Private Sub OnBrowseClick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles BrowseButton.Click
        ' ----------------------------------------------
        ' User has clicked on browse button
        ' ----------------------------------------------
        If OpenFileDialog.ShowDialog() = DialogResult.OK Then
            HelpText = OpenFileDialog.FileName
            Dim FileName As String = OpenFileDialog.FileName
            'If the File is in the same directory as the .apsim file then just use a relative path not an absolute path. Get rid of any directories in the path, just have the filename.
            If (Path.GetDirectoryName(Controller.ApsimData.FileName) <> "") Then    'there will be no path for the .apsim file if the user has not saved yet.
                FileName = FileName.Replace(Path.GetDirectoryName(Controller.ApsimData.FileName) + "\", "") 'replace the directories in the file path with "" IF they match the directory path of the .apsim file.
            End If
            'If the file is in the same directory as the install location for this version of Apsim. Then just use the "%apsuite" macro instead of the installation path.
            'This way you won't need to change it when you upgrade to a new version of Apsim. (eg. APSIMSettings.ApsimDirectory for Apsim version 6.0 would be "C:\Program Files\Apsim6")
            FileName = Configuration.AddMacros(FileName)   'replace the directories in the file path with "%apsuite" IF they match the directory path of the install directory of this version of Apsim.
            XmlHelper.SetValue(Data, "filename", FileName)                  'change the chunk of xml (from the .apsim file) for this node to the new file name.
            Me.OnRefresh()                                                  'refresh the FileUI gui.
        End If
    End Sub


    Private Sub OnActivate(ByVal sender As Object, ByVal e As EventArgs)
        If File.Exists(FullFileName) AndAlso FileDateTime <> File.GetLastWriteTime(FullFileName) Then
            OnRefresh()
        End If
    End Sub

    Public Overrides Sub OnSave()
        If (Not FileContentsBox.ReadOnly) And System.IO.File.Exists(FullFileName) Then
            FileContentsBox.SaveFile(FullFileName, RichTextBoxStreamType.PlainText)
        End If
    End Sub

    Private Sub SearchButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SearchButton.Click
        SearchString(SearchTextBox.Text)
    End Sub
    Private Sub SearchString(ByVal text As String)
        If text.Length > 0 Then
            FileContentsBox.SelectionBackColor = Color.FromKnownColor(KnownColor.Control)
            FileContentsBox.SelectionStart = FileContentsBox.SelectionStart + 1
            Dim indexToText As Integer = FileContentsBox.Find(text, FileContentsBox.SelectionStart, RichTextBoxFinds.None)
            If indexToText = -1 Then
                FileContentsBox.SelectionStart = 0
            End If
            If indexToText >= 0 Then
                FileContentsBox.SelectionBackColor = Color.LightBlue
                FileContentsBox.ScrollToCaret()
            End If
        End If
    End Sub

    Private Sub FileContentsBox_KeyDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles FileContentsBox.KeyDown
        If e.KeyCode.Equals(Keys.F3) Then
            SearchString(SearchTextBox.Text)
        End If
    End Sub


    Private Sub OnSearchBoxKeyDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles SearchTextBox.KeyDown
        If e.KeyCode.Equals(Keys.F3) Or e.KeyCode.Equals(Keys.Return) Then
            SearchString(SearchTextBox.Text)
        End If

    End Sub
End Class
