
Imports Controllers


<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class OperationsUI
    Inherits BaseView

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing AndAlso components IsNot Nothing Then
            components.Dispose()
        End If
        MyBase.Dispose(disposing)
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
      Me.components = New System.ComponentModel.Container
      Me.TabControl = New System.Windows.Forms.TabControl
      Me.TabPage1 = New System.Windows.Forms.TabPage
      Me.TabPage2 = New System.Windows.Forms.TabPage
      Me.StartOfDayGrid = New UIBits.EnhancedGrid
      Me.EndOfDayGrid = New UIBits.EnhancedGrid
      Me.TabControl.SuspendLayout()
      Me.TabPage1.SuspendLayout()
      Me.TabPage2.SuspendLayout()
      CType(Me.StartOfDayGrid, System.ComponentModel.ISupportInitialize).BeginInit()
      CType(Me.EndOfDayGrid, System.ComponentModel.ISupportInitialize).BeginInit()
      Me.SuspendLayout()
      '
      'MyHelpLabel
      '
      Me.MyHelpLabel.Size = New System.Drawing.Size(843, 16)
      '
      'TabControl
      '
      Me.TabControl.Controls.Add(Me.TabPage1)
      Me.TabControl.Controls.Add(Me.TabPage2)
      Me.TabControl.Dock = System.Windows.Forms.DockStyle.Fill
      Me.TabControl.Location = New System.Drawing.Point(0, 16)
      Me.TabControl.Name = "TabControl"
      Me.TabControl.SelectedIndex = 0
      Me.TabControl.Size = New System.Drawing.Size(843, 573)
      Me.TabControl.TabIndex = 3
      '
      'TabPage1
      '
      Me.TabPage1.Controls.Add(Me.StartOfDayGrid)
      Me.TabPage1.Location = New System.Drawing.Point(4, 22)
      Me.TabPage1.Name = "TabPage1"
      Me.TabPage1.Padding = New System.Windows.Forms.Padding(3)
      Me.TabPage1.Size = New System.Drawing.Size(835, 547)
      Me.TabPage1.TabIndex = 0
      Me.TabPage1.Text = "Start of day"
      Me.TabPage1.UseVisualStyleBackColor = True
      '
      'TabPage2
      '
      Me.TabPage2.Controls.Add(Me.EndOfDayGrid)
      Me.TabPage2.Location = New System.Drawing.Point(4, 22)
      Me.TabPage2.Name = "TabPage2"
      Me.TabPage2.Padding = New System.Windows.Forms.Padding(3)
      Me.TabPage2.Size = New System.Drawing.Size(835, 547)
      Me.TabPage2.TabIndex = 1
      Me.TabPage2.Text = "End of day"
      Me.TabPage2.UseVisualStyleBackColor = True
      '
      'StartOfDayGrid
      '
      Me.StartOfDayGrid.AllowUserToAddRows = False
      Me.StartOfDayGrid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
      Me.StartOfDayGrid.DataSourceTable = Nothing
      Me.StartOfDayGrid.Dock = System.Windows.Forms.DockStyle.Fill
      Me.StartOfDayGrid.Location = New System.Drawing.Point(3, 3)
      Me.StartOfDayGrid.Name = "StartOfDayGrid"
      Me.StartOfDayGrid.RowHeadersVisible = False
      Me.StartOfDayGrid.Size = New System.Drawing.Size(829, 541)
      Me.StartOfDayGrid.TabIndex = 0
      '
      'EndOfDayGrid
      '
      Me.EndOfDayGrid.AllowUserToAddRows = False
      Me.EndOfDayGrid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
      Me.EndOfDayGrid.DataSourceTable = Nothing
      Me.EndOfDayGrid.Dock = System.Windows.Forms.DockStyle.Fill
      Me.EndOfDayGrid.Location = New System.Drawing.Point(3, 3)
      Me.EndOfDayGrid.Name = "EndOfDayGrid"
      Me.EndOfDayGrid.RowHeadersVisible = False
      Me.EndOfDayGrid.Size = New System.Drawing.Size(829, 541)
      Me.EndOfDayGrid.TabIndex = 0
      '
      'OperationsUI
      '
      Me.Controls.Add(Me.TabControl)
      Me.Name = "OperationsUI"
      Me.Size = New System.Drawing.Size(843, 589)
      Me.Controls.SetChildIndex(Me.MyHelpLabel, 0)
      Me.Controls.SetChildIndex(Me.TabControl, 0)
      Me.TabControl.ResumeLayout(False)
      Me.TabPage1.ResumeLayout(False)
      Me.TabPage2.ResumeLayout(False)
      CType(Me.StartOfDayGrid, System.ComponentModel.ISupportInitialize).EndInit()
      CType(Me.EndOfDayGrid, System.ComponentModel.ISupportInitialize).EndInit()
      Me.ResumeLayout(False)

   End Sub
   Friend WithEvents TabControl As System.Windows.Forms.TabControl
   Friend WithEvents TabPage1 As System.Windows.Forms.TabPage
   Friend WithEvents TabPage2 As System.Windows.Forms.TabPage
   Friend WithEvents StartOfDayGrid As UIBits.EnhancedGrid
   Friend WithEvents EndOfDayGrid As UIBits.EnhancedGrid

End Class
