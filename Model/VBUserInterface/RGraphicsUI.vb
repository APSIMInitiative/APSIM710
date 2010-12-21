
Imports System.Collections
Imports System.Collections.Specialized
Imports System.Collections.Generic
Imports System.IO
Imports System.Xml
Imports Microsoft.Win32

Imports FarPoint.Win.Spread

Imports Controllers
Imports CSGeneral
Imports CSGeneral.Utility

Imports UIBits      'InputDialog

Public Class RGraphicsUI
    Inherits BaseView
    Private InRefresh As Boolean

    ' The image from R
    Friend WithEvents PictureBox As System.Windows.Forms.PictureBox

    ' An editable script 
    Friend WithEvents ScriptPage As System.Windows.Forms.TabPage
    Private ScriptBox As QWhale.Editor.SyntaxEdit

    ' Text output from running R
    Friend WithEvents Console As System.Windows.Forms.TabPage
    Private ConsoleBox As System.Windows.Forms.TextBox

#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        InRefresh = False
        'Add any initialization after the InitializeComponent() call
        ScriptBox = New QWhale.Editor.SyntaxEdit

        ScriptBox.WordWrap = False
        ScriptBox.Gutter.Options = CType((((QWhale.Editor.GutterOptions.PaintLineNumbers Or QWhale.Editor.GutterOptions.PaintLinesOnGutter) _
                            Or QWhale.Editor.GutterOptions.PaintBookMarks) _
                            Or QWhale.Editor.GutterOptions.PaintLineModificators), QWhale.Editor.GutterOptions)
        ScriptBox.Dock = DockStyle.Fill
        ScriptBox.BringToFront()
        Dim TabStops() As Integer = {3}
        ScriptBox.Lines.TabStops = TabStops
        ScriptBox.Lines.UseSpaces = True

        ScriptPage.Controls.Add(ScriptBox)

        ConsoleBox = New Windows.Forms.TextBox
        ConsoleBox.Dock = DockStyle.Fill
        ConsoleBox.Font = New Font("Courier New", 12)
        ConsoleBox.WordWrap = False
        ConsoleBox.Multiline = True
        ConsoleBox.ScrollBars = ScrollBars.Both
        ConsoleBox.BringToFront()

        Console.Controls.Add(ConsoleBox)
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
    Friend WithEvents ImagePage As System.Windows.Forms.TabPage
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.TabControl = New System.Windows.Forms.TabControl
        Me.ImagePage = New System.Windows.Forms.TabPage
        Me.ScriptPage = New System.Windows.Forms.TabPage
        Me.PictureBox = New System.Windows.Forms.PictureBox
        Me.Console = New System.Windows.Forms.TabPage
        Me.TabControl.SuspendLayout()
        Me.ImagePage.SuspendLayout()
        CType(Me.PictureBox, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'MyHelpLabel
        '
        Me.MyHelpLabel.Size = New System.Drawing.Size(1022, 16)
        '
        'TabControl
        '
        Me.TabControl.Controls.Add(Me.ImagePage)
        Me.TabControl.Controls.Add(Me.ScriptPage)
        Me.TabControl.Controls.Add(Me.Console)
        Me.TabControl.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TabControl.Location = New System.Drawing.Point(0, 16)
        Me.TabControl.Name = "TabControl"
        Me.TabControl.SelectedIndex = 0
        Me.TabControl.Size = New System.Drawing.Size(1022, 800)
        Me.TabControl.TabIndex = 3
        '
        'ImagePage
        '
        Me.ImagePage.Controls.Add(Me.PictureBox)
        Me.ImagePage.Location = New System.Drawing.Point(4, 22)
        Me.ImagePage.Name = "ImagePage"
        Me.ImagePage.Size = New System.Drawing.Size(1014, 774)
        Me.ImagePage.TabIndex = 0
        Me.ImagePage.Text = "Graph"
        Me.ImagePage.UseVisualStyleBackColor = True
        '
        'ScriptPage
        '
        Me.ScriptPage.Location = New System.Drawing.Point(4, 22)
        Me.ScriptPage.Name = "ScriptPage"
        Me.ScriptPage.Size = New System.Drawing.Size(1014, 774)
        Me.ScriptPage.TabIndex = 1
        Me.ScriptPage.Text = "Script"
        Me.ScriptPage.UseVisualStyleBackColor = True
        '
        'PictureBox
        '
        Me.PictureBox.Dock = System.Windows.Forms.DockStyle.Left
        Me.PictureBox.Location = New System.Drawing.Point(0, 0)
        Me.PictureBox.Name = "PictureBox"
        Me.PictureBox.Size = New System.Drawing.Size(255, 774)
        Me.PictureBox.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize
        Me.PictureBox.TabIndex = 2
        Me.PictureBox.TabStop = False
        '
        'Console
        '
        Me.Console.Location = New System.Drawing.Point(4, 22)
        Me.Console.Name = "Console"
        Me.Console.Padding = New System.Windows.Forms.Padding(3)
        Me.Console.Size = New System.Drawing.Size(1014, 774)
        Me.Console.TabIndex = 2
        Me.Console.Text = "Console"
        Me.Console.UseVisualStyleBackColor = True
        '
        'RGraphicsUI
        '
        Me.Controls.Add(Me.TabControl)
        Me.Name = "RGraphicsUI"
        Me.Size = New System.Drawing.Size(1022, 816)
        Me.Controls.SetChildIndex(Me.MyHelpLabel, 0)
        Me.Controls.SetChildIndex(Me.TabControl, 0)
        Me.TabControl.ResumeLayout(False)
        Me.ImagePage.ResumeLayout(False)
        Me.ImagePage.PerformLayout()
        CType(Me.PictureBox, System.ComponentModel.ISupportInitialize).EndInit()
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

        Dim OutputFileNames As New List(Of String)

        UIUtility.OutputFileUtility.GetOutputFiles(Controller, Controller.Selection, OutputFileNames)

        Dim Script As String = ""
        For Each ScriptNode As XmlNode In XmlHelper.ChildNodes(Data, "script")
            Script = Script + XmlHelper.Value(ScriptNode, "text")
        Next
        ScriptBox.Text = Script
        ConsoleBox.Text = ""

        Dim canRun As Boolean = True
        Dim needsRerun As Boolean = False
        Dim imageFile As String = Directory.GetCurrentDirectory() + "\" + Me.Name() + ".png"
        If (Not (File.Exists(imageFile))) Then
            needsRerun = True
        Else
            ' See if a simulation has been run that invalidates the image
            Dim myDate As Date = File.GetCreationTime(imageFile)
            For Each outputfile As String In OutputFileNames
                If (Not (File.Exists(outputfile))) Then
                    canRun = False
                Else
                    If (canRun And File.GetCreationTime(outputfile) > myDate) Then
                        needsRerun = True
                    End If
                End If
            Next
            ' See if the window size has changed
            Dim diskImage As Image = Image.FromFile(imageFile)
            If ((Me.TabControl.Size.Width() <> diskImage.Width) Or _
                (Me.TabControl.Size.Height() <> diskImage.Height)) Then
                needsRerun = True
            End If
        End If

        If (canRun And needsRerun) Then
            Dim fp As New FileStream(Me.Name() + ".R", FileMode.Create, FileAccess.Write)
            Dim s As New StreamWriter(fp)
            s.WriteLine("# Automatically generated - do not edit")
            s.WriteLine("width<- " + Me.TabControl.Size.Width().ToString())
            s.WriteLine("height<- " + Me.TabControl.Size.Height().ToString())
            s.WriteLine("imageFileName <- """ + Replace(imageFile, "\", "/") + """")
            s.Write("inputFiles <- c(")
            For Each outputfile As String In OutputFileNames
                s.Write("""" + Replace(outputfile, "\", "/") + """")
            Next
            s.WriteLine(")")
            s.WriteLine(Script)
            s.Close()
            fp.Close()

            ' scrub the old image so that we can be sure it's regenerated
            File.Delete(imageFile)

            ' try and run R with this script
            Dim regKey As RegistryKey = Registry.LocalMachine.OpenSubKey("SOFTWARE\R-core\R", True)
            Dim rpath As String = regKey.GetValue("InstallPath", "")

            ' Should test for pre 2.12.x that doesnt have rscript installed
            Dim rcmd As String = rpath + "\bin\Rscript.exe"
            Dim args As String = "--slave --vanilla """ + Directory.GetCurrentDirectory() + "\" + Me.Name() + ".R"""
            Dim p As System.Diagnostics.Process = RunProcess(rcmd, args, Directory.GetCurrentDirectory())

            p.WaitForExit()

            Dim msg As String = "Command:   " + rcmd + vbCrLf
            msg += "Arguments: " + args + vbCrLf
            msg += "stdout: " + vbCrLf + p.StandardOutput.ReadToEnd() + vbCrLf
            msg += "stderr: " + vbCrLf + p.StandardError.ReadToEnd() + vbCrLf
            msg += "script: " + vbCrLf + Script
            Me.ConsoleBox.Text = msg

            ' update displayed image
            If (File.Exists(imageFile)) Then
                Me.PictureBox.Image = Image.FromFile(imageFile)
            End If
        End If

        InRefresh = False
    End Sub

    Public Overrides Sub OnSave()
        ' --------------------------------------
        ' Save the script box if it has changed.
        ' --------------------------------------
        Data.RemoveAll()

        Dim ScriptNode As XmlNode = Data.AppendChild(Data.OwnerDocument.CreateElement("script"))
        XmlHelper.SetName(ScriptNode, "script")
        XmlHelper.SetValue(ScriptNode, "text", ScriptBox.Text)

    End Sub
End Class
