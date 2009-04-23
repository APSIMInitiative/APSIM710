
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
        Dim TipAppearance1 As FarPoint.Win.Spread.TipAppearance = New FarPoint.Win.Spread.TipAppearance
        Me.Spread = New FarPoint.Win.Spread.FpSpread
        Me.StartDayGrid = New FarPoint.Win.Spread.SheetView
        Me.EndDayGrid = New FarPoint.Win.Spread.SheetView
        CType(Me.Spread, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.StartDayGrid, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.EndDayGrid, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'Spread
        '
        Me.Spread.AccessibleDescription = "Spread, Start of day, Row 0, Column 0, "
        Me.Spread.Dock = System.Windows.Forms.DockStyle.Fill
        Me.Spread.Location = New System.Drawing.Point(0, 40)
        Me.Spread.Name = "Spread"
        Me.Spread.Sheets.AddRange(New FarPoint.Win.Spread.SheetView() {Me.StartDayGrid, Me.EndDayGrid})
        Me.Spread.Size = New System.Drawing.Size(843, 549)
        Me.Spread.TabIndex = 2
        TipAppearance1.BackColor = System.Drawing.SystemColors.Info
        TipAppearance1.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        TipAppearance1.ForeColor = System.Drawing.SystemColors.InfoText
        Me.Spread.TextTipAppearance = TipAppearance1
        '
        'StartDayGrid
        '
        Me.StartDayGrid.Reset()
        'Formulas and custom names must be loaded with R1C1 reference style
        Me.StartDayGrid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1
        Me.StartDayGrid.ColumnCount = 2
        Me.StartDayGrid.RowHeader.ColumnCount = 0
        Me.StartDayGrid.AutoUpdateNotes = True
        Me.StartDayGrid.ColumnHeader.Cells.Get(0, 0).Value = "Date "
        Me.StartDayGrid.ColumnHeader.Cells.Get(0, 1).Value = "Operation"
        Me.StartDayGrid.Columns.Get(0).Label = "Date "
        Me.StartDayGrid.Columns.Get(0).Width = 125.0!
        Me.StartDayGrid.Columns.Get(1).Label = "Operation"
        Me.StartDayGrid.Columns.Get(1).Width = 487.0!
        Me.StartDayGrid.RowHeader.Columns.Default.Resizable = True
        Me.StartDayGrid.RowHeader.Visible = False
        Me.StartDayGrid.SheetName = "Start of day"
        Me.StartDayGrid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1
        '
        'EndDayGrid
        '
        Me.EndDayGrid.Reset()
        'Formulas and custom names must be loaded with R1C1 reference style
        Me.EndDayGrid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1
        Me.EndDayGrid.ColumnCount = 2
        Me.EndDayGrid.RowHeader.ColumnCount = 0
        Me.EndDayGrid.AutoUpdateNotes = True
        Me.EndDayGrid.ColumnHeader.Cells.Get(0, 0).Value = "Date"
        Me.EndDayGrid.ColumnHeader.Cells.Get(0, 1).Value = "Operation"
        Me.EndDayGrid.Columns.Get(0).Label = "Date"
        Me.EndDayGrid.Columns.Get(0).Width = 119.0!
        Me.EndDayGrid.Columns.Get(1).Label = "Operation"
        Me.EndDayGrid.Columns.Get(1).Width = 467.0!
        Me.EndDayGrid.RowHeader.Columns.Default.Resizable = False
        Me.EndDayGrid.SheetName = "End of day"
        Me.EndDayGrid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1
        '
        'OperationsUI
        '
        Me.Controls.Add(Me.Spread)
        Me.Name = "OperationsUI"
        Me.Size = New System.Drawing.Size(843, 589)
        Me.Controls.SetChildIndex(Me.Spread, 0)
        CType(Me.Spread, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.StartDayGrid, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.EndDayGrid, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents Spread As FarPoint.Win.Spread.FpSpread
    Friend WithEvents StartDayGrid As FarPoint.Win.Spread.SheetView
    Friend WithEvents EndDayGrid As FarPoint.Win.Spread.SheetView

End Class
