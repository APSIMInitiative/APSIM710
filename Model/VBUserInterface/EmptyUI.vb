
Imports System.Collections
Imports System.Collections.Specialized

Imports ApsimFile
Imports Controllers
Imports CSGeneral


Public Class EmptyUI
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
    Friend WithEvents Panel2 As System.Windows.Forms.Panel
    Friend WithEvents PictureBox As System.Windows.Forms.PictureBox
    Friend WithEvents Panel1 As System.Windows.Forms.Panel
    Friend WithEvents Label1 As System.Windows.Forms.Label
   Friend WithEvents GroupBox As System.Windows.Forms.GroupBox
   Friend WithEvents DocumentationLink As System.Windows.Forms.LinkLabel
   Friend WithEvents MainLabel As System.Windows.Forms.Label
   <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
      Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(EmptyUI))
      Me.Panel2 = New System.Windows.Forms.Panel
      Me.Panel1 = New System.Windows.Forms.Panel
      Me.GroupBox = New System.Windows.Forms.GroupBox
      Me.DocumentationLink = New System.Windows.Forms.LinkLabel
      Me.Label1 = New System.Windows.Forms.Label
      Me.MainLabel = New System.Windows.Forms.Label
      Me.PictureBox = New System.Windows.Forms.PictureBox
      Me.Panel2.SuspendLayout()
      Me.Panel1.SuspendLayout()
      Me.GroupBox.SuspendLayout()
      CType(Me.PictureBox, System.ComponentModel.ISupportInitialize).BeginInit()
      Me.SuspendLayout()
      '
      'MyHelpLabel
      '
      Me.MyHelpLabel.Size = New System.Drawing.Size(753, 16)
      '
      'Panel2
      '
      Me.Panel2.Controls.Add(Me.Panel1)
      Me.Panel2.Controls.Add(Me.PictureBox)
      Me.Panel2.Dock = System.Windows.Forms.DockStyle.Fill
      Me.Panel2.Location = New System.Drawing.Point(0, 16)
      Me.Panel2.Name = "Panel2"
      Me.Panel2.Size = New System.Drawing.Size(753, 793)
      Me.Panel2.TabIndex = 8
      '
      'Panel1
      '
      Me.Panel1.Controls.Add(Me.GroupBox)
      Me.Panel1.Controls.Add(Me.Label1)
      Me.Panel1.Controls.Add(Me.MainLabel)
      Me.Panel1.Dock = System.Windows.Forms.DockStyle.Fill
      Me.Panel1.Location = New System.Drawing.Point(255, 0)
      Me.Panel1.Name = "Panel1"
      Me.Panel1.Size = New System.Drawing.Size(498, 793)
      Me.Panel1.TabIndex = 7
      '
      'GroupBox
      '
      Me.GroupBox.Controls.Add(Me.DocumentationLink)
      Me.GroupBox.Location = New System.Drawing.Point(32, 211)
      Me.GroupBox.Name = "GroupBox"
      Me.GroupBox.Size = New System.Drawing.Size(330, 288)
      Me.GroupBox.TabIndex = 9
      Me.GroupBox.TabStop = False
      '
      'DocumentationLink
      '
      Me.DocumentationLink.Location = New System.Drawing.Point(6, 25)
      Me.DocumentationLink.Name = "DocumentationLink"
      Me.DocumentationLink.Size = New System.Drawing.Size(208, 24)
      Me.DocumentationLink.TabIndex = 8
      Me.DocumentationLink.TabStop = True
      Me.DocumentationLink.Text = "See Module Documentation for details."
      '
      'Label1
      '
      Me.Label1.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
                  Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
      Me.Label1.Location = New System.Drawing.Point(16, 64)
      Me.Label1.Name = "Label1"
      Me.Label1.Size = New System.Drawing.Size(435, 88)
      Me.Label1.TabIndex = 8
      Me.Label1.Text = "This component does not require extra user input. "
      '
      'MainLabel
      '
      Me.MainLabel.Font = New System.Drawing.Font("Microsoft Sans Serif", 14.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
      Me.MainLabel.Location = New System.Drawing.Point(16, 24)
      Me.MainLabel.Name = "MainLabel"
      Me.MainLabel.Size = New System.Drawing.Size(208, 22)
      Me.MainLabel.TabIndex = 6
      Me.MainLabel.Text = "Crop type"
      '
      'PictureBox
      '
      Me.PictureBox.Dock = System.Windows.Forms.DockStyle.Left
      Me.PictureBox.Image = CType(resources.GetObject("PictureBox.Image"), System.Drawing.Image)
      Me.PictureBox.Location = New System.Drawing.Point(0, 0)
      Me.PictureBox.Name = "PictureBox"
      Me.PictureBox.Size = New System.Drawing.Size(255, 793)
      Me.PictureBox.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize
      Me.PictureBox.TabIndex = 1
      Me.PictureBox.TabStop = False
      '
      'EmptyUI
      '
      Me.Controls.Add(Me.Panel2)
      Me.Name = "EmptyUI"
      Me.Size = New System.Drawing.Size(753, 809)
      Me.Controls.SetChildIndex(Me.MyHelpLabel, 0)
      Me.Controls.SetChildIndex(Me.Panel2, 0)
      Me.Panel2.ResumeLayout(False)
      Me.Panel2.PerformLayout()
      Me.Panel1.ResumeLayout(False)
      Me.GroupBox.ResumeLayout(False)
      CType(Me.PictureBox, System.ComponentModel.ISupportInitialize).EndInit()
      Me.ResumeLayout(False)

   End Sub

#End Region

   Overrides Sub OnRefresh()
      MainLabel.Text = XmlHelper.Type(Data)
      Me.HelpText = "This module does not have any editable properties."

      Dim imagefile As String = Types.Instance.MetaData(Data.Name, "image")
      If System.IO.File.Exists(imagefile) Then
         PictureBox.Image = Image.FromFile(imagefile)
      Else
         PictureBox.Image = Nothing
      End If

      ' Populate the group box will links for each document file found.
      Dim PosX As Integer = 30
      Dim PosY As Integer = 30
      Dim LineSpacing As Integer = 20
      GroupBox.Controls.Clear()
      Dim DocNames As New List(Of String)
      Dim Urls As New List(Of String)
      Types.Instance.Documentation(XmlHelper.Type(Data), DocNames, Urls)
      For i As Integer = 0 To DocNames.Count - 1
         Dim NewLink As New LinkLabel()
         NewLink.Text = DocNames(i)
         NewLink.Tag = Urls(i)
         AddHandler NewLink.LinkClicked, AddressOf OnLinkClicked
         NewLink.Parent = GroupBox
         NewLink.AutoSize = True
         NewLink.Top = PosY
         NewLink.Left = PosX
         PosY = PosY + LineSpacing
      Next
      GroupBox.Visible = DocNames.Count > 0
   End Sub

   Private Sub OnLinkClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.LinkLabelLinkClickedEventArgs)
      Try
         Dim url As String = sender.Tag
         Process.Start(url)
      Catch ex As Exception
         MessageBox.Show(ex.Message)
      End Try
   End Sub

End Class
