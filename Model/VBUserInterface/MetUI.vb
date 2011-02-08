
Imports System
Imports System.IO

Imports CSGeneral
Imports ApsimFile
Imports Controllers


Public Class MetUI
    Inherits BaseView

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
    Friend WithEvents MetGraphControl1 As MetGraphControl
    Friend WithEvents btnBrowse As System.Windows.Forms.Button
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(MetUI))
        Me.ImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.OpenFileDialog = New System.Windows.Forms.OpenFileDialog
        Me.btnBrowse = New System.Windows.Forms.Button
        Me.MetGraphControl1 = New VBUserInterface.MetGraphControl
        Me.SuspendLayout()
        '
        'MyHelpLabel
        '
        Me.MyHelpLabel.Size = New System.Drawing.Size(764, 16)
        '
        'ImageList
        '
        Me.ImageList.ImageStream = CType(resources.GetObject("ImageList.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.ImageList.TransparentColor = System.Drawing.Color.Transparent
        Me.ImageList.Images.SetKeyName(0, "")
        '
        'OpenFileDialog
        '
        Me.OpenFileDialog.Filter = "met file (*.met)|*.met|All files(*.*)|*.*"
        Me.OpenFileDialog.RestoreDirectory = True
        '
        'btnBrowse
        '
        Me.btnBrowse.BackColor = System.Drawing.SystemColors.Info
        Me.btnBrowse.FlatStyle = System.Windows.Forms.FlatStyle.Popup
        Me.btnBrowse.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft
        Me.btnBrowse.ImageIndex = 0
        Me.btnBrowse.ImageList = Me.ImageList
        Me.btnBrowse.Location = New System.Drawing.Point(0, 20)
        Me.btnBrowse.Name = "btnBrowse"
        Me.btnBrowse.Size = New System.Drawing.Size(88, 29)
        Me.btnBrowse.TabIndex = 13
        Me.btnBrowse.Text = "Browse ..."
        Me.btnBrowse.TextAlign = System.Drawing.ContentAlignment.MiddleRight
        Me.btnBrowse.UseVisualStyleBackColor = False
        '
        'MetGraphControl1
        '
        Me.MetGraphControl1.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
                    Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.MetGraphControl1.AutoScroll = True
        Me.MetGraphControl1.BackColor = System.Drawing.SystemColors.Control
        Me.MetGraphControl1.HelpText = ""
        Me.MetGraphControl1.Location = New System.Drawing.Point(0, 50)
        Me.MetGraphControl1.Name = "MetGraphControl1"
        Me.MetGraphControl1.Size = New System.Drawing.Size(764, 481)
        Me.MetGraphControl1.TabIndex = 14
        '
        'MetUI
        '
        Me.Controls.Add(Me.btnBrowse)
        Me.Controls.Add(Me.MetGraphControl1)
        Me.Name = "MetUI"
        Me.Size = New System.Drawing.Size(764, 526)
        Me.Controls.SetChildIndex(Me.MyHelpLabel, 0)
        Me.Controls.SetChildIndex(Me.MetGraphControl1, 0)
        Me.Controls.SetChildIndex(Me.btnBrowse, 0)
        Me.ResumeLayout(False)

    End Sub

#End Region

    Protected Overrides Sub OnLoad()
        MetGraphControl1.OnLoad(Controller, NodePath, Data.OuterXml) 'Controller.Selection.Contents)
    End Sub
    Public Overrides Sub OnRefresh()
        Dim FileName As String = XmlHelper.Value(Data, "filename")

        MetGraphControl1.OnRefresh()
        HelpText = FileName
        OpenFileDialog.InitialDirectory = Path.GetDirectoryName(FileName)
        MetGraphControl1.SetFileName(FileName)
    End Sub

    Public Overrides Sub OnSave()
        MetGraphControl1.OnSave()
        Dim Doc As New Xml.XmlDocument()
        Doc.LoadXml(MetGraphControl1.GetData())
        Data.InnerXml = Doc.DocumentElement.InnerXml
    End Sub

    Private Sub btnBrowse_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles btnBrowse.Paint
        ControlPaint.DrawBorder3D(e.Graphics, e.ClipRectangle, Border3DStyle.Etched)
    End Sub

    Private Sub btnBrowse_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnBrowse.Click
        Try
            If OpenFileDialog.ShowDialog() = DialogResult.OK Then
                MetGraphControl1.SetFileName(OpenFileDialog.FileName)
                Me.HelpText = MetGraphControl1.GetFileName()
            End If
        Catch ex As Exception
            MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try

    End Sub

End Class
