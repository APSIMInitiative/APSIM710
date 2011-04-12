<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class ExcelExport
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.fileListBox = New System.Windows.Forms.CheckedListBox()
        Me.bSelectAll = New System.Windows.Forms.Button()
        Me.bOK = New System.Windows.Forms.Button()
        Me.bCancel = New System.Windows.Forms.Button()
        Me.bDeselectAll = New System.Windows.Forms.Button()
        Me.SuspendLayout()
        '
        'fileListBox
        '
        Me.fileListBox.CheckOnClick = True
        Me.fileListBox.FormattingEnabled = True
        Me.fileListBox.HorizontalScrollbar = True
        Me.fileListBox.Location = New System.Drawing.Point(4, 8)
        Me.fileListBox.Name = "fileListBox"
        Me.fileListBox.Size = New System.Drawing.Size(425, 344)
        Me.fileListBox.TabIndex = 0
        '
        'bSelectAll
        '
        Me.bSelectAll.Location = New System.Drawing.Point(13, 465)
        Me.bSelectAll.Name = "bSelectAll"
        Me.bSelectAll.Size = New System.Drawing.Size(87, 30)
        Me.bSelectAll.TabIndex = 1
        Me.bSelectAll.Text = "Select All"
        Me.bSelectAll.UseVisualStyleBackColor = True
        '
        'bOK
        '
        Me.bOK.Location = New System.Drawing.Point(261, 465)
        Me.bOK.Name = "bOK"
        Me.bOK.Size = New System.Drawing.Size(81, 30)
        Me.bOK.TabIndex = 3
        Me.bOK.Text = "OK"
        Me.bOK.UseVisualStyleBackColor = True
        '
        'bCancel
        '
        Me.bCancel.Location = New System.Drawing.Point(348, 465)
        Me.bCancel.Name = "bCancel"
        Me.bCancel.Size = New System.Drawing.Size(81, 30)
        Me.bCancel.TabIndex = 4
        Me.bCancel.Text = "Cancel"
        Me.bCancel.UseVisualStyleBackColor = True
        '
        'bDeselectAll
        '
        Me.bDeselectAll.Location = New System.Drawing.Point(106, 465)
        Me.bDeselectAll.Name = "bDeselectAll"
        Me.bDeselectAll.Size = New System.Drawing.Size(92, 30)
        Me.bDeselectAll.TabIndex = 5
        Me.bDeselectAll.Text = "Deselect All"
        Me.bDeselectAll.UseVisualStyleBackColor = True
        '
        'ExcelExport
        '
        Me.AcceptButton = Me.bOK
        Me.AutoScaleDimensions = New System.Drawing.SizeF(8.0!, 16.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.CancelButton = Me.bCancel
        Me.ClientSize = New System.Drawing.Size(441, 507)
        Me.Controls.Add(Me.bDeselectAll)
        Me.Controls.Add(Me.bCancel)
        Me.Controls.Add(Me.bOK)
        Me.Controls.Add(Me.bSelectAll)
        Me.Controls.Add(Me.fileListBox)
        Me.Name = "ExcelExport"
        Me.ShowIcon = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent
        Me.Text = "ExcelExport"
        Me.TopMost = True
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents fileListBox As System.Windows.Forms.CheckedListBox
    Friend WithEvents bSelectAll As System.Windows.Forms.Button
    Friend WithEvents bOK As System.Windows.Forms.Button
    Friend WithEvents bCancel As System.Windows.Forms.Button
    Friend WithEvents bDeselectAll As System.Windows.Forms.Button
End Class
