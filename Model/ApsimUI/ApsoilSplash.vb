

Public Class ApsoilSplash
    Inherits System.Windows.Forms.Form

#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()



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
    Friend WithEvents PictureBoxTop As System.Windows.Forms.PictureBox
    Friend WithEvents OkButton As System.Windows.Forms.Button

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(ApsoilSplash))
        Me.PictureBoxTop = New System.Windows.Forms.PictureBox
        Me.OkButton = New System.Windows.Forms.Button
        CType(Me.PictureBoxTop, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'PictureBoxTop
        '
        Me.PictureBoxTop.BackColor = System.Drawing.Color.White
        Me.PictureBoxTop.Image = CType(resources.GetObject("PictureBoxTop.Image"), System.Drawing.Image)
        Me.PictureBoxTop.Location = New System.Drawing.Point(0, 0)
        Me.PictureBoxTop.Name = "PictureBoxTop"
        Me.PictureBoxTop.Size = New System.Drawing.Size(631, 600)
        Me.PictureBoxTop.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize
        Me.PictureBoxTop.TabIndex = 15
        Me.PictureBoxTop.TabStop = False
        '
        'OkButton
        '
        Me.OkButton.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.OkButton.AutoSize = True
        Me.OkButton.DialogResult = System.Windows.Forms.DialogResult.OK
        Me.OkButton.Location = New System.Drawing.Point(543, 12)
        Me.OkButton.Name = "OkButton"
        Me.OkButton.Size = New System.Drawing.Size(75, 23)
        Me.OkButton.TabIndex = 16
        Me.OkButton.Text = "Ok"
        Me.OkButton.UseVisualStyleBackColor = True
        '
        'ApsoilSplash
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.AutoSize = True
        Me.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink
        Me.ClientSize = New System.Drawing.Size(630, 585)
        Me.ControlBox = False
        Me.Controls.Add(Me.OkButton)
        Me.Controls.Add(Me.PictureBoxTop)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle
        Me.Location = New System.Drawing.Point(100, 100)
        Me.Name = "ApsoilSplash"
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.TopMost = True
        CType(Me.PictureBoxTop, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

#End Region

End Class
