
Imports System.Math
Imports System.IO
Imports System.Drawing
Imports System.Xml

Imports ApsimFile
Imports Controllers
Imports CSGeneral
Imports VBMet
Imports Steema.TeeChart.Styles




Public Class MetGraphControl
    Inherits BaseView

    Private Metfile As New APSIMInputFile
    Private MetData As New DataTable
    Private MonthlyData As New DataTable
    Private YearlyData As DataView
    Private StartDate As DateTime
    Private EndDate As DateTime
    Private FileName As String
    Private StartRow As Integer
    Private EndRow As Integer
    Friend WithEvents TabImages As System.Windows.Forms.ImageList
    Friend WithEvents TabControl As System.Windows.Forms.TabControl
    Friend WithEvents TabPage1 As System.Windows.Forms.TabPage
    Friend WithEvents ContentsBox As System.Windows.Forms.TextBox
    Friend WithEvents TabPage2 As System.Windows.Forms.TabPage
    Friend WithEvents RainfallChart As Steema.TeeChart.TChart
    Friend WithEvents TabPage3 As System.Windows.Forms.TabPage
    Friend WithEvents TabPage5 As System.Windows.Forms.TabPage
    Friend WithEvents TabPage4 As System.Windows.Forms.TabPage
    Friend WithEvents RainfallBar As Steema.TeeChart.Styles.Bar
    Friend WithEvents MonthlyRainfallChart As Steema.TeeChart.TChart
    Friend WithEvents TemperatureChart As Steema.TeeChart.TChart
    Friend WithEvents RadiationChart As Steema.TeeChart.TChart
    Friend WithEvents RainfallBar2 As Steema.TeeChart.Styles.Bar
    Friend WithEvents MaximumTemperatureLine As Steema.TeeChart.Styles.Line
    Friend WithEvents MinimumTemperatureLine As Steema.TeeChart.Styles.Line
    Friend WithEvents RadiationLine As Steema.TeeChart.Styles.Line
    Friend WithEvents MaximumRadiationLine As Steema.TeeChart.Styles.Line
    Friend WithEvents MonthlyRainfallBar As Steema.TeeChart.Styles.Bar
    Friend WithEvents MonthlyEvaporationLine As Steema.TeeChart.Styles.Line
    Friend WithEvents YearStartBox As System.Windows.Forms.NumericUpDown
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents RainfallLabel As System.Windows.Forms.Label
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents NumYearsBox As System.Windows.Forms.NumericUpDown
    Friend WithEvents YearPanel As System.Windows.Forms.Panel
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents Label5 As System.Windows.Forms.Label
    Private CurrentShortCut As Shortcut

#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()
        InitializeComponent()
    End Sub

    'UserControl overrides dispose to clean up the component list.
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
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(MetGraphControl))
        Me.TabImages = New System.Windows.Forms.ImageList(Me.components)
        Me.TabControl = New System.Windows.Forms.TabControl
        Me.TabPage1 = New System.Windows.Forms.TabPage
        Me.YearPanel = New System.Windows.Forms.Panel
        Me.Label5 = New System.Windows.Forms.Label
        Me.Label4 = New System.Windows.Forms.Label
        Me.YearStartBox = New System.Windows.Forms.NumericUpDown
        Me.NumYearsBox = New System.Windows.Forms.NumericUpDown
        Me.Label3 = New System.Windows.Forms.Label
        Me.ContentsBox = New System.Windows.Forms.TextBox
        Me.TabPage2 = New System.Windows.Forms.TabPage
        Me.Label1 = New System.Windows.Forms.Label
        Me.RainfallLabel = New System.Windows.Forms.Label
        Me.RainfallChart = New Steema.TeeChart.TChart
        Me.RainfallBar = New Steema.TeeChart.Styles.Bar
        Me.TabPage3 = New System.Windows.Forms.TabPage
        Me.MonthlyRainfallChart = New Steema.TeeChart.TChart
        Me.MonthlyRainfallBar = New Steema.TeeChart.Styles.Bar
        Me.MonthlyEvaporationLine = New Steema.TeeChart.Styles.Line
        Me.TabPage5 = New System.Windows.Forms.TabPage
        Me.TemperatureChart = New Steema.TeeChart.TChart
        Me.MaximumTemperatureLine = New Steema.TeeChart.Styles.Line
        Me.MinimumTemperatureLine = New Steema.TeeChart.Styles.Line
        Me.TabPage4 = New System.Windows.Forms.TabPage
        Me.RadiationChart = New Steema.TeeChart.TChart
        Me.RainfallBar2 = New Steema.TeeChart.Styles.Bar
        Me.RadiationLine = New Steema.TeeChart.Styles.Line
        Me.MaximumRadiationLine = New Steema.TeeChart.Styles.Line
        Me.TabControl.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        Me.YearPanel.SuspendLayout()
        CType(Me.YearStartBox, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.NumYearsBox, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabPage2.SuspendLayout()
        Me.TabPage3.SuspendLayout()
        Me.TabPage5.SuspendLayout()
        Me.TabPage4.SuspendLayout()
        Me.SuspendLayout()
        '
        'MyHelpLabel
        '
        Me.MyHelpLabel.Size = New System.Drawing.Size(1015, 16)
        '
        'TabImages
        '
        Me.TabImages.ImageStream = CType(resources.GetObject("TabImages.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.TabImages.TransparentColor = System.Drawing.Color.Transparent
        Me.TabImages.Images.SetKeyName(0, "text.png")
        Me.TabImages.Images.SetKeyName(1, "drink_blue.png")
        Me.TabImages.Images.SetKeyName(2, "thermometer.png")
        Me.TabImages.Images.SetKeyName(3, "sunglasses.png")
        '
        'TabControl
        '
        Me.TabControl.Controls.Add(Me.TabPage1)
        Me.TabControl.Controls.Add(Me.TabPage2)
        Me.TabControl.Controls.Add(Me.TabPage3)
        Me.TabControl.Controls.Add(Me.TabPage5)
        Me.TabControl.Controls.Add(Me.TabPage4)
        Me.TabControl.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TabControl.ImageList = Me.TabImages
        Me.TabControl.Location = New System.Drawing.Point(0, 16)
        Me.TabControl.Name = "TabControl"
        Me.TabControl.SelectedIndex = 0
        Me.TabControl.Size = New System.Drawing.Size(1015, 708)
        Me.TabControl.TabIndex = 15
        '
        'TabPage1
        '
        Me.TabPage1.Controls.Add(Me.YearPanel)
        Me.TabPage1.Controls.Add(Me.ContentsBox)
        Me.TabPage1.ImageIndex = 0
        Me.TabPage1.Location = New System.Drawing.Point(4, 23)
        Me.TabPage1.Name = "TabPage1"
        Me.TabPage1.Padding = New System.Windows.Forms.Padding(3)
        Me.TabPage1.Size = New System.Drawing.Size(1007, 681)
        Me.TabPage1.TabIndex = 0
        Me.TabPage1.Text = "Raw data"
        Me.TabPage1.UseVisualStyleBackColor = True
        '
        'YearPanel
        '
        Me.YearPanel.BackColor = System.Drawing.Color.White
        Me.YearPanel.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.YearPanel.Controls.Add(Me.Label5)
        Me.YearPanel.Controls.Add(Me.Label4)
        Me.YearPanel.Controls.Add(Me.YearStartBox)
        Me.YearPanel.Controls.Add(Me.NumYearsBox)
        Me.YearPanel.Controls.Add(Me.Label3)
        Me.YearPanel.Location = New System.Drawing.Point(6, 26)
        Me.YearPanel.Name = "YearPanel"
        Me.YearPanel.Size = New System.Drawing.Size(171, 60)
        Me.YearPanel.TabIndex = 19
        '
        'Label5
        '
        Me.Label5.AutoSize = True
        Me.Label5.Location = New System.Drawing.Point(3, 36)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(34, 13)
        Me.Label5.TabIndex = 20
        Me.Label5.Text = "Show"
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.Location = New System.Drawing.Point(3, 7)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(32, 13)
        Me.Label4.TabIndex = 19
        Me.Label4.Text = "Year:"
        '
        'YearStartBox
        '
        Me.YearStartBox.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.YearStartBox.CausesValidation = False
        Me.YearStartBox.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.YearStartBox.Location = New System.Drawing.Point(41, 5)
        Me.YearStartBox.Maximum = New Decimal(New Integer() {2050, 0, 0, 0})
        Me.YearStartBox.Minimum = New Decimal(New Integer() {1900, 0, 0, 0})
        Me.YearStartBox.Name = "YearStartBox"
        Me.YearStartBox.Size = New System.Drawing.Size(64, 21)
        Me.YearStartBox.TabIndex = 16
        Me.YearStartBox.Value = New Decimal(New Integer() {2000, 0, 0, 0})
        '
        'NumYearsBox
        '
        Me.NumYearsBox.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.NumYearsBox.CausesValidation = False
        Me.NumYearsBox.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.NumYearsBox.Location = New System.Drawing.Point(41, 32)
        Me.NumYearsBox.Maximum = New Decimal(New Integer() {200, 0, 0, 0})
        Me.NumYearsBox.Minimum = New Decimal(New Integer() {1, 0, 0, 0})
        Me.NumYearsBox.Name = "NumYearsBox"
        Me.NumYearsBox.Size = New System.Drawing.Size(49, 21)
        Me.NumYearsBox.TabIndex = 17
        Me.NumYearsBox.Value = New Decimal(New Integer() {1, 0, 0, 0})
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(96, 36)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(74, 13)
        Me.Label3.TabIndex = 18
        Me.Label3.Text = "year(s) of data"
        '
        'ContentsBox
        '
        Me.ContentsBox.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.ContentsBox.Dock = System.Windows.Forms.DockStyle.Fill
        Me.ContentsBox.Font = New System.Drawing.Font("Courier New", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.ContentsBox.Location = New System.Drawing.Point(3, 3)
        Me.ContentsBox.Multiline = True
        Me.ContentsBox.Name = "ContentsBox"
        Me.ContentsBox.ReadOnly = True
        Me.ContentsBox.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.ContentsBox.Size = New System.Drawing.Size(1001, 675)
        Me.ContentsBox.TabIndex = 0
        Me.ContentsBox.WordWrap = False
        '
        'TabPage2
        '
        Me.TabPage2.Controls.Add(Me.Label1)
        Me.TabPage2.Controls.Add(Me.RainfallLabel)
        Me.TabPage2.Controls.Add(Me.RainfallChart)
        Me.TabPage2.ImageIndex = 1
        Me.TabPage2.Location = New System.Drawing.Point(4, 23)
        Me.TabPage2.Name = "TabPage2"
        Me.TabPage2.Padding = New System.Windows.Forms.Padding(3)
        Me.TabPage2.Size = New System.Drawing.Size(1007, 681)
        Me.TabPage2.TabIndex = 1
        Me.TabPage2.Text = "Rainfall chart"
        Me.TabPage2.UseVisualStyleBackColor = True
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(303, 11)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(72, 13)
        Me.Label1.TabIndex = 16
        Me.Label1.Text = "Total Rainfall:"
        '
        'RainfallLabel
        '
        Me.RainfallLabel.AutoSize = True
        Me.RainfallLabel.Location = New System.Drawing.Point(381, 11)
        Me.RainfallLabel.Name = "RainfallLabel"
        Me.RainfallLabel.Size = New System.Drawing.Size(19, 13)
        Me.RainfallLabel.TabIndex = 15
        Me.RainfallLabel.Text = "L1"
        '
        'RainfallChart
        '
        '
        '
        '
        Me.RainfallChart.Aspect.ElevationFloat = 345
        Me.RainfallChart.Aspect.RotationFloat = 345
        Me.RainfallChart.Aspect.View3D = False
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Axes.Bottom.Automatic = True
        '
        '
        '
        Me.RainfallChart.Axes.Bottom.AxisPen.Width = 1
        '
        '
        '
        Me.RainfallChart.Axes.Bottom.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash
        Me.RainfallChart.Axes.Bottom.Grid.Visible = False
        Me.RainfallChart.Axes.Bottom.Grid.ZPosition = 0
        Me.RainfallChart.Axes.Bottom.Increment = 30
        '
        '
        '
        Me.RainfallChart.Axes.Bottom.Labels.CustomSize = 40
        Me.RainfallChart.Axes.Bottom.Labels.DateTimeFormat = "MMM yyyy"
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Axes.Bottom.Labels.Font.Shadow.Visible = False
        Me.RainfallChart.Axes.Bottom.Labels.Font.Size = 11
        '
        '
        '
        Me.RainfallChart.Axes.Bottom.Labels.Shadow.Visible = False
        Me.RainfallChart.Axes.Bottom.MaximumOffset = 2
        '
        '
        '
        Me.RainfallChart.Axes.Bottom.MinorTicks.Visible = False
        '
        '
        '
        Me.RainfallChart.Axes.Bottom.Ticks.Length = 0
        '
        '
        '
        Me.RainfallChart.Axes.Bottom.TicksInner.Length = 5
        '
        '
        '
        Me.RainfallChart.Axes.Bottom.Title.Caption = "Date"
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Axes.Bottom.Title.Font.Shadow.Visible = False
        Me.RainfallChart.Axes.Bottom.Title.Font.Size = 11
        Me.RainfallChart.Axes.Bottom.Title.Lines = New String() {"Date"}
        '
        '
        '
        Me.RainfallChart.Axes.Bottom.Title.Shadow.Visible = False
        '
        '
        '
        Me.RainfallChart.Axes.Depth.Automatic = True
        '
        '
        '
        Me.RainfallChart.Axes.Depth.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash
        Me.RainfallChart.Axes.Depth.Grid.ZPosition = 0
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Axes.Depth.Labels.Font.Shadow.Visible = False
        '
        '
        '
        Me.RainfallChart.Axes.Depth.Labels.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Axes.Depth.Title.Font.Shadow.Visible = False
        '
        '
        '
        Me.RainfallChart.Axes.Depth.Title.Shadow.Visible = False
        '
        '
        '
        Me.RainfallChart.Axes.DepthTop.Automatic = True
        '
        '
        '
        Me.RainfallChart.Axes.DepthTop.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash
        Me.RainfallChart.Axes.DepthTop.Grid.ZPosition = 0
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Axes.DepthTop.Labels.Font.Shadow.Visible = False
        '
        '
        '
        Me.RainfallChart.Axes.DepthTop.Labels.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Axes.DepthTop.Title.Font.Shadow.Visible = False
        '
        '
        '
        Me.RainfallChart.Axes.DepthTop.Title.Shadow.Visible = False
        '
        '
        '
        Me.RainfallChart.Axes.Left.Automatic = True
        '
        '
        '
        Me.RainfallChart.Axes.Left.AxisPen.Width = 1
        '
        '
        '
        Me.RainfallChart.Axes.Left.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash
        Me.RainfallChart.Axes.Left.Grid.Visible = False
        Me.RainfallChart.Axes.Left.Grid.ZPosition = 0
        '
        '
        '
        Me.RainfallChart.Axes.Left.Labels.CustomSize = 40
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Axes.Left.Labels.Font.Shadow.Visible = False
        Me.RainfallChart.Axes.Left.Labels.Font.Size = 11
        Me.RainfallChart.Axes.Left.Labels.Separation = 20
        '
        '
        '
        Me.RainfallChart.Axes.Left.Labels.Shadow.Visible = False
        '
        '
        '
        Me.RainfallChart.Axes.Left.MinorTicks.Visible = False
        '
        '
        '
        Me.RainfallChart.Axes.Left.Ticks.Length = 0
        '
        '
        '
        Me.RainfallChart.Axes.Left.TicksInner.Length = 5
        '
        '
        '
        Me.RainfallChart.Axes.Left.Title.Caption = "Rainfall (mm)"
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Axes.Left.Title.Font.Shadow.Visible = False
        Me.RainfallChart.Axes.Left.Title.Font.Size = 11
        Me.RainfallChart.Axes.Left.Title.Lines = New String() {"Rainfall (mm)"}
        '
        '
        '
        Me.RainfallChart.Axes.Left.Title.Shadow.Visible = False
        '
        '
        '
        Me.RainfallChart.Axes.Right.Automatic = True
        '
        '
        '
        Me.RainfallChart.Axes.Right.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash
        Me.RainfallChart.Axes.Right.Grid.ZPosition = 0
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Axes.Right.Labels.Font.Shadow.Visible = False
        '
        '
        '
        Me.RainfallChart.Axes.Right.Labels.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Axes.Right.Title.Font.Shadow.Visible = False
        '
        '
        '
        Me.RainfallChart.Axes.Right.Title.Shadow.Visible = False
        Me.RainfallChart.Axes.Right.Visible = False
        '
        '
        '
        Me.RainfallChart.Axes.Top.Automatic = True
        '
        '
        '
        Me.RainfallChart.Axes.Top.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash
        Me.RainfallChart.Axes.Top.Grid.ZPosition = 0
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Axes.Top.Labels.Font.Shadow.Visible = False
        '
        '
        '
        Me.RainfallChart.Axes.Top.Labels.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Axes.Top.Title.Font.Shadow.Visible = False
        '
        '
        '
        Me.RainfallChart.Axes.Top.Title.Shadow.Visible = False
        Me.RainfallChart.Axes.Top.Visible = False
        Me.RainfallChart.BackColor = System.Drawing.Color.Transparent
        Me.RainfallChart.Cursor = System.Windows.Forms.Cursors.Default
        Me.RainfallChart.Dock = System.Windows.Forms.DockStyle.Fill
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Footer.Font.Shadow.Visible = False
        '
        '
        '
        Me.RainfallChart.Footer.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Header.Font.Shadow.Visible = False
        Me.RainfallChart.Header.Lines = New String() {""}
        '
        '
        '
        Me.RainfallChart.Header.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Legend.Font.Shadow.Visible = False
        Me.RainfallChart.Legend.LegendStyle = Steema.TeeChart.LegendStyles.Series
        '
        '
        '
        Me.RainfallChart.Legend.Pen.Visible = False
        '
        '
        '
        Me.RainfallChart.Legend.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Legend.Title.Font.Bold = True
        '
        '
        '
        Me.RainfallChart.Legend.Title.Font.Shadow.Visible = False
        '
        '
        '
        Me.RainfallChart.Legend.Title.Pen.Visible = False
        '
        '
        '
        Me.RainfallChart.Legend.Title.Shadow.Visible = False
        Me.RainfallChart.Legend.Visible = False
        Me.RainfallChart.Location = New System.Drawing.Point(3, 3)
        Me.RainfallChart.Name = "RainfallChart"
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Panel.Bevel.Outer = Steema.TeeChart.Drawing.BevelStyles.None
        '
        '
        '
        Me.RainfallChart.Panel.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(0, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        '
        '
        '
        Me.RainfallChart.Panel.ImageBevel.Width = 1
        Me.RainfallChart.Panel.MarginBottom = 1
        Me.RainfallChart.Panel.MarginLeft = 1
        '
        '
        '
        Me.RainfallChart.Panel.Shadow.Visible = False
        Me.RainfallChart.Series.Add(Me.RainfallBar)
        Me.RainfallChart.Size = New System.Drawing.Size(1001, 675)
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.SubFooter.Font.Shadow.Visible = False
        '
        '
        '
        Me.RainfallChart.SubFooter.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.SubHeader.Font.Shadow.Visible = False
        '
        '
        '
        Me.RainfallChart.SubHeader.Shadow.Visible = False
        Me.RainfallChart.TabIndex = 14
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Walls.Back.AutoHide = False
        '
        '
        '
        Me.RainfallChart.Walls.Back.Shadow.Visible = False
        '
        '
        '
        Me.RainfallChart.Walls.Bottom.AutoHide = False
        '
        '
        '
        Me.RainfallChart.Walls.Bottom.Shadow.Visible = False
        '
        '
        '
        Me.RainfallChart.Walls.Left.AutoHide = False
        '
        '
        '
        Me.RainfallChart.Walls.Left.Shadow.Visible = False
        '
        '
        '
        Me.RainfallChart.Walls.Right.AutoHide = False
        '
        '
        '
        Me.RainfallChart.Walls.Right.Shadow.Visible = False
        Me.RainfallChart.Walls.Visible = False
        '
        'RainfallBar
        '
        '
        '
        '
        Me.RainfallBar.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(0, Byte), Integer), CType(CType(192, Byte), Integer), CType(CType(192, Byte), Integer))
        '
        '
        '
        '
        '
        '
        Me.RainfallBar.Marks.Callout.ArrowHead = Steema.TeeChart.Styles.ArrowHeadStyles.None
        Me.RainfallBar.Marks.Callout.ArrowHeadSize = 8
        '
        '
        '
        Me.RainfallBar.Marks.Callout.Brush.Color = System.Drawing.Color.Black
        Me.RainfallBar.Marks.Callout.Distance = 0
        Me.RainfallBar.Marks.Callout.Draw3D = False
        Me.RainfallBar.Marks.Callout.Length = 20
        Me.RainfallBar.Marks.Callout.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle
        '
        '
        '
        '
        '
        '
        Me.RainfallBar.Marks.Font.Shadow.Visible = False
        Me.RainfallBar.Marks.Visible = False
        '
        '
        '
        Me.RainfallBar.Pen.Color = System.Drawing.Color.FromArgb(CType(CType(0, Byte), Integer), CType(CType(115, Byte), Integer), CType(CType(115, Byte), Integer))
        Me.RainfallBar.Title = "Rainfall"
        '
        '
        '
        Me.RainfallBar.XValues.DataMember = "X"
        Me.RainfallBar.XValues.DateTime = True
        Me.RainfallBar.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending
        '
        '
        '
        Me.RainfallBar.YValues.DataMember = "Bar"
        '
        'TabPage3
        '
        Me.TabPage3.Controls.Add(Me.MonthlyRainfallChart)
        Me.TabPage3.ImageIndex = 1
        Me.TabPage3.Location = New System.Drawing.Point(4, 23)
        Me.TabPage3.Name = "TabPage3"
        Me.TabPage3.Padding = New System.Windows.Forms.Padding(3)
        Me.TabPage3.Size = New System.Drawing.Size(1007, 681)
        Me.TabPage3.TabIndex = 2
        Me.TabPage3.Text = "Monthly rainfall chart"
        Me.TabPage3.UseVisualStyleBackColor = True
        '
        'MonthlyRainfallChart
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Aspect.ElevationFloat = 345
        Me.MonthlyRainfallChart.Aspect.RotationFloat = 345
        Me.MonthlyRainfallChart.Aspect.View3D = False
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Bottom.Automatic = True
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Bottom.AxisPen.Width = 1
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Bottom.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash
        Me.MonthlyRainfallChart.Axes.Bottom.Grid.Visible = False
        Me.MonthlyRainfallChart.Axes.Bottom.Grid.ZPosition = 0
        Me.MonthlyRainfallChart.Axes.Bottom.Increment = 30
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Bottom.Labels.CustomSize = 40
        Me.MonthlyRainfallChart.Axes.Bottom.Labels.DateTimeFormat = "MMM yyyy"
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Bottom.Labels.Font.Shadow.Visible = False
        Me.MonthlyRainfallChart.Axes.Bottom.Labels.Font.Size = 11
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Bottom.Labels.Shadow.Visible = False
        Me.MonthlyRainfallChart.Axes.Bottom.MaximumOffset = 2
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Bottom.MinorTicks.Visible = False
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Bottom.Ticks.Length = 0
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Bottom.TicksInner.Length = 5
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Bottom.Title.Caption = "Date"
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Bottom.Title.Font.Shadow.Visible = False
        Me.MonthlyRainfallChart.Axes.Bottom.Title.Font.Size = 11
        Me.MonthlyRainfallChart.Axes.Bottom.Title.Lines = New String() {"Date"}
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Bottom.Title.Shadow.Visible = False
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Depth.Automatic = True
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Depth.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash
        Me.MonthlyRainfallChart.Axes.Depth.Grid.ZPosition = 0
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Depth.Labels.Font.Shadow.Visible = False
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Depth.Labels.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Depth.Title.Font.Shadow.Visible = False
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Depth.Title.Shadow.Visible = False
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.DepthTop.Automatic = True
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.DepthTop.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash
        Me.MonthlyRainfallChart.Axes.DepthTop.Grid.ZPosition = 0
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.DepthTop.Labels.Font.Shadow.Visible = False
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.DepthTop.Labels.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.DepthTop.Title.Font.Shadow.Visible = False
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.DepthTop.Title.Shadow.Visible = False
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Left.Automatic = True
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Left.AxisPen.Width = 1
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Left.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash
        Me.MonthlyRainfallChart.Axes.Left.Grid.Visible = False
        Me.MonthlyRainfallChart.Axes.Left.Grid.ZPosition = 0
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Left.Labels.CustomSize = 40
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Left.Labels.Font.Shadow.Visible = False
        Me.MonthlyRainfallChart.Axes.Left.Labels.Font.Size = 11
        Me.MonthlyRainfallChart.Axes.Left.Labels.Separation = 20
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Left.Labels.Shadow.Visible = False
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Left.MinorTicks.Visible = False
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Left.Ticks.Length = 0
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Left.TicksInner.Length = 5
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Left.Title.Caption = "Rainfall and Evaporation (mm)"
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Left.Title.Font.Shadow.Visible = False
        Me.MonthlyRainfallChart.Axes.Left.Title.Font.Size = 11
        Me.MonthlyRainfallChart.Axes.Left.Title.Lines = New String() {"Rainfall and Evaporation (mm)"}
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Left.Title.Shadow.Visible = False
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Right.Automatic = True
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Right.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash
        Me.MonthlyRainfallChart.Axes.Right.Grid.ZPosition = 0
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Right.Labels.Font.Shadow.Visible = False
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Right.Labels.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Right.Title.Font.Shadow.Visible = False
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Right.Title.Shadow.Visible = False
        Me.MonthlyRainfallChart.Axes.Right.Visible = False
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Top.Automatic = True
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Top.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash
        Me.MonthlyRainfallChart.Axes.Top.Grid.ZPosition = 0
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Top.Labels.Font.Shadow.Visible = False
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Top.Labels.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Top.Title.Font.Shadow.Visible = False
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Top.Title.Shadow.Visible = False
        Me.MonthlyRainfallChart.Axes.Top.Visible = False
        Me.MonthlyRainfallChart.BackColor = System.Drawing.Color.Transparent
        Me.MonthlyRainfallChart.Cursor = System.Windows.Forms.Cursors.Default
        Me.MonthlyRainfallChart.Dock = System.Windows.Forms.DockStyle.Fill
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Footer.Font.Shadow.Visible = False
        '
        '
        '
        Me.MonthlyRainfallChart.Footer.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Header.Font.Shadow.Visible = False
        Me.MonthlyRainfallChart.Header.Lines = New String() {""}
        '
        '
        '
        Me.MonthlyRainfallChart.Header.Shadow.Visible = False
        '
        '
        '
        Me.MonthlyRainfallChart.Legend.Alignment = Steema.TeeChart.LegendAlignments.Bottom
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Legend.Font.Shadow.Visible = False
        Me.MonthlyRainfallChart.Legend.Font.Size = 11
        Me.MonthlyRainfallChart.Legend.LegendStyle = Steema.TeeChart.LegendStyles.Series
        '
        '
        '
        Me.MonthlyRainfallChart.Legend.Pen.Visible = False
        '
        '
        '
        Me.MonthlyRainfallChart.Legend.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Legend.Title.Font.Bold = True
        '
        '
        '
        Me.MonthlyRainfallChart.Legend.Title.Font.Shadow.Visible = False
        '
        '
        '
        Me.MonthlyRainfallChart.Legend.Title.Pen.Visible = False
        '
        '
        '
        Me.MonthlyRainfallChart.Legend.Title.Shadow.Visible = False
        Me.MonthlyRainfallChart.Location = New System.Drawing.Point(3, 3)
        Me.MonthlyRainfallChart.Name = "MonthlyRainfallChart"
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Panel.Bevel.Outer = Steema.TeeChart.Drawing.BevelStyles.None
        '
        '
        '
        Me.MonthlyRainfallChart.Panel.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(0, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        '
        '
        '
        Me.MonthlyRainfallChart.Panel.ImageBevel.Width = 1
        Me.MonthlyRainfallChart.Panel.MarginBottom = 1
        Me.MonthlyRainfallChart.Panel.MarginLeft = 1
        '
        '
        '
        Me.MonthlyRainfallChart.Panel.Shadow.Visible = False
        Me.MonthlyRainfallChart.Series.Add(Me.MonthlyRainfallBar)
        Me.MonthlyRainfallChart.Series.Add(Me.MonthlyEvaporationLine)
        Me.MonthlyRainfallChart.Size = New System.Drawing.Size(1001, 675)
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.SubFooter.Font.Shadow.Visible = False
        '
        '
        '
        Me.MonthlyRainfallChart.SubFooter.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.SubHeader.Font.Shadow.Visible = False
        '
        '
        '
        Me.MonthlyRainfallChart.SubHeader.Shadow.Visible = False
        Me.MonthlyRainfallChart.TabIndex = 15
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Walls.Back.AutoHide = False
        '
        '
        '
        Me.MonthlyRainfallChart.Walls.Back.Shadow.Visible = False
        '
        '
        '
        Me.MonthlyRainfallChart.Walls.Bottom.AutoHide = False
        '
        '
        '
        Me.MonthlyRainfallChart.Walls.Bottom.Shadow.Visible = False
        '
        '
        '
        Me.MonthlyRainfallChart.Walls.Left.AutoHide = False
        '
        '
        '
        Me.MonthlyRainfallChart.Walls.Left.Shadow.Visible = False
        '
        '
        '
        Me.MonthlyRainfallChart.Walls.Right.AutoHide = False
        '
        '
        '
        Me.MonthlyRainfallChart.Walls.Right.Shadow.Visible = False
        Me.MonthlyRainfallChart.Walls.Visible = False
        '
        'MonthlyRainfallBar
        '
        '
        '
        '
        Me.MonthlyRainfallBar.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(0, Byte), Integer), CType(CType(192, Byte), Integer), CType(CType(192, Byte), Integer))
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallBar.Marks.Callout.ArrowHead = Steema.TeeChart.Styles.ArrowHeadStyles.None
        Me.MonthlyRainfallBar.Marks.Callout.ArrowHeadSize = 8
        '
        '
        '
        Me.MonthlyRainfallBar.Marks.Callout.Brush.Color = System.Drawing.Color.Black
        Me.MonthlyRainfallBar.Marks.Callout.Distance = 0
        Me.MonthlyRainfallBar.Marks.Callout.Draw3D = False
        Me.MonthlyRainfallBar.Marks.Callout.Length = 20
        Me.MonthlyRainfallBar.Marks.Callout.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallBar.Marks.Font.Shadow.Visible = False
        Me.MonthlyRainfallBar.Marks.Visible = False
        '
        '
        '
        Me.MonthlyRainfallBar.Pen.Color = System.Drawing.Color.FromArgb(CType(CType(0, Byte), Integer), CType(CType(115, Byte), Integer), CType(CType(115, Byte), Integer))
        Me.MonthlyRainfallBar.Title = "Rainfall"
        '
        '
        '
        Me.MonthlyRainfallBar.XValues.DataMember = "X"
        Me.MonthlyRainfallBar.XValues.DateTime = True
        Me.MonthlyRainfallBar.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending
        '
        '
        '
        Me.MonthlyRainfallBar.YValues.DataMember = "Bar"
        '
        'MonthlyEvaporationLine
        '
        '
        '
        '
        Me.MonthlyEvaporationLine.Brush.Color = System.Drawing.Color.Red
        '
        '
        '
        Me.MonthlyEvaporationLine.LinePen.Color = System.Drawing.Color.FromArgb(CType(CType(153, Byte), Integer), CType(CType(0, Byte), Integer), CType(CType(0, Byte), Integer))
        '
        '
        '
        '
        '
        '
        Me.MonthlyEvaporationLine.Marks.Callout.ArrowHead = Steema.TeeChart.Styles.ArrowHeadStyles.None
        Me.MonthlyEvaporationLine.Marks.Callout.ArrowHeadSize = 8
        '
        '
        '
        Me.MonthlyEvaporationLine.Marks.Callout.Brush.Color = System.Drawing.Color.Black
        Me.MonthlyEvaporationLine.Marks.Callout.Distance = 0
        Me.MonthlyEvaporationLine.Marks.Callout.Draw3D = False
        Me.MonthlyEvaporationLine.Marks.Callout.Length = 10
        Me.MonthlyEvaporationLine.Marks.Callout.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle
        '
        '
        '
        '
        '
        '
        Me.MonthlyEvaporationLine.Marks.Font.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        Me.MonthlyEvaporationLine.Pointer.Brush.Color = System.Drawing.Color.Red
        Me.MonthlyEvaporationLine.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle
        Me.MonthlyEvaporationLine.Title = "Evaporation"
        '
        '
        '
        Me.MonthlyEvaporationLine.XValues.DataMember = "X"
        Me.MonthlyEvaporationLine.XValues.DateTime = True
        Me.MonthlyEvaporationLine.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending
        '
        '
        '
        Me.MonthlyEvaporationLine.YValues.DataMember = "Y"
        '
        'TabPage5
        '
        Me.TabPage5.Controls.Add(Me.TemperatureChart)
        Me.TabPage5.ImageIndex = 2
        Me.TabPage5.Location = New System.Drawing.Point(4, 23)
        Me.TabPage5.Name = "TabPage5"
        Me.TabPage5.Size = New System.Drawing.Size(1007, 681)
        Me.TabPage5.TabIndex = 4
        Me.TabPage5.Text = "Temperature chart"
        Me.TabPage5.UseVisualStyleBackColor = True
        '
        'TemperatureChart
        '
        '
        '
        '
        Me.TemperatureChart.Aspect.ElevationFloat = 345
        Me.TemperatureChart.Aspect.RotationFloat = 345
        Me.TemperatureChart.Aspect.View3D = False
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Axes.Bottom.Automatic = True
        '
        '
        '
        Me.TemperatureChart.Axes.Bottom.AxisPen.Width = 1
        '
        '
        '
        Me.TemperatureChart.Axes.Bottom.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash
        Me.TemperatureChart.Axes.Bottom.Grid.Visible = False
        Me.TemperatureChart.Axes.Bottom.Grid.ZPosition = 0
        Me.TemperatureChart.Axes.Bottom.Increment = 30
        '
        '
        '
        Me.TemperatureChart.Axes.Bottom.Labels.DateTimeFormat = "MMM yyyy"
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Axes.Bottom.Labels.Font.Shadow.Visible = False
        Me.TemperatureChart.Axes.Bottom.Labels.Font.Size = 11
        '
        '
        '
        Me.TemperatureChart.Axes.Bottom.Labels.Shadow.Visible = False
        '
        '
        '
        Me.TemperatureChart.Axes.Bottom.MinorTicks.Visible = False
        '
        '
        '
        Me.TemperatureChart.Axes.Bottom.Ticks.Length = 0
        '
        '
        '
        Me.TemperatureChart.Axes.Bottom.TicksInner.Length = 5
        '
        '
        '
        Me.TemperatureChart.Axes.Bottom.Title.Caption = "Date"
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Axes.Bottom.Title.Font.Shadow.Visible = False
        Me.TemperatureChart.Axes.Bottom.Title.Font.Size = 11
        Me.TemperatureChart.Axes.Bottom.Title.Lines = New String() {"Date"}
        '
        '
        '
        Me.TemperatureChart.Axes.Bottom.Title.Shadow.Visible = False
        '
        '
        '
        Me.TemperatureChart.Axes.Depth.Automatic = True
        '
        '
        '
        Me.TemperatureChart.Axes.Depth.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash
        Me.TemperatureChart.Axes.Depth.Grid.ZPosition = 0
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Axes.Depth.Labels.Font.Shadow.Visible = False
        '
        '
        '
        Me.TemperatureChart.Axes.Depth.Labels.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Axes.Depth.Title.Font.Shadow.Visible = False
        '
        '
        '
        Me.TemperatureChart.Axes.Depth.Title.Shadow.Visible = False
        '
        '
        '
        Me.TemperatureChart.Axes.DepthTop.Automatic = True
        '
        '
        '
        Me.TemperatureChart.Axes.DepthTop.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash
        Me.TemperatureChart.Axes.DepthTop.Grid.ZPosition = 0
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Axes.DepthTop.Labels.Font.Shadow.Visible = False
        '
        '
        '
        Me.TemperatureChart.Axes.DepthTop.Labels.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Axes.DepthTop.Title.Font.Shadow.Visible = False
        '
        '
        '
        Me.TemperatureChart.Axes.DepthTop.Title.Shadow.Visible = False
        '
        '
        '
        Me.TemperatureChart.Axes.Left.Automatic = True
        '
        '
        '
        Me.TemperatureChart.Axes.Left.AxisPen.Width = 1
        '
        '
        '
        Me.TemperatureChart.Axes.Left.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash
        Me.TemperatureChart.Axes.Left.Grid.Visible = False
        Me.TemperatureChart.Axes.Left.Grid.ZPosition = 0
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Axes.Left.Labels.Font.Shadow.Visible = False
        Me.TemperatureChart.Axes.Left.Labels.Font.Size = 11
        Me.TemperatureChart.Axes.Left.Labels.Separation = 20
        '
        '
        '
        Me.TemperatureChart.Axes.Left.Labels.Shadow.Visible = False
        '
        '
        '
        Me.TemperatureChart.Axes.Left.MinorTicks.Visible = False
        '
        '
        '
        Me.TemperatureChart.Axes.Left.Ticks.Length = 0
        '
        '
        '
        Me.TemperatureChart.Axes.Left.TicksInner.Length = 5
        '
        '
        '
        Me.TemperatureChart.Axes.Left.Title.Caption = "Temperature (oC)"
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Axes.Left.Title.Font.Shadow.Visible = False
        Me.TemperatureChart.Axes.Left.Title.Font.Size = 11
        Me.TemperatureChart.Axes.Left.Title.Lines = New String() {"Temperature (oC)"}
        '
        '
        '
        Me.TemperatureChart.Axes.Left.Title.Shadow.Visible = False
        '
        '
        '
        Me.TemperatureChart.Axes.Right.Automatic = True
        '
        '
        '
        Me.TemperatureChart.Axes.Right.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash
        Me.TemperatureChart.Axes.Right.Grid.ZPosition = 0
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Axes.Right.Labels.Font.Shadow.Visible = False
        '
        '
        '
        Me.TemperatureChart.Axes.Right.Labels.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Axes.Right.Title.Font.Shadow.Visible = False
        '
        '
        '
        Me.TemperatureChart.Axes.Right.Title.Shadow.Visible = False
        Me.TemperatureChart.Axes.Right.Visible = False
        '
        '
        '
        Me.TemperatureChart.Axes.Top.Automatic = True
        '
        '
        '
        Me.TemperatureChart.Axes.Top.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash
        Me.TemperatureChart.Axes.Top.Grid.ZPosition = 0
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Axes.Top.Labels.Font.Shadow.Visible = False
        '
        '
        '
        Me.TemperatureChart.Axes.Top.Labels.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Axes.Top.Title.Font.Shadow.Visible = False
        '
        '
        '
        Me.TemperatureChart.Axes.Top.Title.Shadow.Visible = False
        Me.TemperatureChart.Axes.Top.Visible = False
        Me.TemperatureChart.BackColor = System.Drawing.Color.Transparent
        Me.TemperatureChart.Cursor = System.Windows.Forms.Cursors.Default
        Me.TemperatureChart.Dock = System.Windows.Forms.DockStyle.Fill
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Footer.Font.Shadow.Visible = False
        '
        '
        '
        Me.TemperatureChart.Footer.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Header.Font.Shadow.Visible = False
        Me.TemperatureChart.Header.Lines = New String() {""}
        '
        '
        '
        Me.TemperatureChart.Header.Shadow.Visible = False
        '
        '
        '
        Me.TemperatureChart.Legend.Alignment = Steema.TeeChart.LegendAlignments.Bottom
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Legend.Font.Shadow.Visible = False
        Me.TemperatureChart.Legend.Font.Size = 11
        Me.TemperatureChart.Legend.LegendStyle = Steema.TeeChart.LegendStyles.Series
        '
        '
        '
        Me.TemperatureChart.Legend.Pen.Visible = False
        '
        '
        '
        Me.TemperatureChart.Legend.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Legend.Title.Font.Bold = True
        '
        '
        '
        Me.TemperatureChart.Legend.Title.Font.Shadow.Visible = False
        '
        '
        '
        Me.TemperatureChart.Legend.Title.Pen.Visible = False
        '
        '
        '
        Me.TemperatureChart.Legend.Title.Shadow.Visible = False
        Me.TemperatureChart.Location = New System.Drawing.Point(0, 0)
        Me.TemperatureChart.Name = "TemperatureChart"
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Panel.Bevel.Outer = Steema.TeeChart.Drawing.BevelStyles.None
        '
        '
        '
        Me.TemperatureChart.Panel.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(0, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        '
        '
        '
        Me.TemperatureChart.Panel.ImageBevel.Width = 1
        '
        '
        '
        Me.TemperatureChart.Panel.Shadow.Visible = False
        Me.TemperatureChart.Series.Add(Me.MaximumTemperatureLine)
        Me.TemperatureChart.Series.Add(Me.MinimumTemperatureLine)
        Me.TemperatureChart.Size = New System.Drawing.Size(1007, 681)
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.SubFooter.Font.Shadow.Visible = False
        '
        '
        '
        Me.TemperatureChart.SubFooter.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.SubHeader.Font.Shadow.Visible = False
        '
        '
        '
        Me.TemperatureChart.SubHeader.Shadow.Visible = False
        Me.TemperatureChart.TabIndex = 16
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Walls.Back.AutoHide = False
        '
        '
        '
        Me.TemperatureChart.Walls.Back.Shadow.Visible = False
        '
        '
        '
        Me.TemperatureChart.Walls.Bottom.AutoHide = False
        '
        '
        '
        Me.TemperatureChart.Walls.Bottom.Shadow.Visible = False
        '
        '
        '
        Me.TemperatureChart.Walls.Left.AutoHide = False
        '
        '
        '
        Me.TemperatureChart.Walls.Left.Shadow.Visible = False
        '
        '
        '
        Me.TemperatureChart.Walls.Right.AutoHide = False
        '
        '
        '
        Me.TemperatureChart.Walls.Right.Shadow.Visible = False
        Me.TemperatureChart.Walls.Visible = False
        '
        'MaximumTemperatureLine
        '
        '
        '
        '
        Me.MaximumTemperatureLine.Brush.Color = System.Drawing.Color.Red
        '
        '
        '
        Me.MaximumTemperatureLine.LinePen.Color = System.Drawing.Color.FromArgb(CType(CType(153, Byte), Integer), CType(CType(0, Byte), Integer), CType(CType(0, Byte), Integer))
        '
        '
        '
        '
        '
        '
        Me.MaximumTemperatureLine.Marks.Callout.ArrowHead = Steema.TeeChart.Styles.ArrowHeadStyles.None
        Me.MaximumTemperatureLine.Marks.Callout.ArrowHeadSize = 8
        '
        '
        '
        Me.MaximumTemperatureLine.Marks.Callout.Brush.Color = System.Drawing.Color.Black
        Me.MaximumTemperatureLine.Marks.Callout.Distance = 0
        Me.MaximumTemperatureLine.Marks.Callout.Draw3D = False
        Me.MaximumTemperatureLine.Marks.Callout.Length = 10
        Me.MaximumTemperatureLine.Marks.Callout.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle
        '
        '
        '
        '
        '
        '
        Me.MaximumTemperatureLine.Marks.Font.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        Me.MaximumTemperatureLine.Pointer.Brush.Color = System.Drawing.Color.Red
        Me.MaximumTemperatureLine.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle
        Me.MaximumTemperatureLine.Title = "Maximum temperature"
        '
        '
        '
        Me.MaximumTemperatureLine.XValues.DataMember = "X"
        Me.MaximumTemperatureLine.XValues.DateTime = True
        Me.MaximumTemperatureLine.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending
        '
        '
        '
        Me.MaximumTemperatureLine.YValues.DataMember = "Y"
        '
        'MinimumTemperatureLine
        '
        '
        '
        '
        Me.MinimumTemperatureLine.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(0, Byte), Integer), CType(CType(0, Byte), Integer), CType(CType(192, Byte), Integer))
        '
        '
        '
        Me.MinimumTemperatureLine.LinePen.Color = System.Drawing.Color.FromArgb(CType(CType(0, Byte), Integer), CType(CType(0, Byte), Integer), CType(CType(115, Byte), Integer))
        '
        '
        '
        '
        '
        '
        Me.MinimumTemperatureLine.Marks.Callout.ArrowHead = Steema.TeeChart.Styles.ArrowHeadStyles.None
        Me.MinimumTemperatureLine.Marks.Callout.ArrowHeadSize = 8
        '
        '
        '
        Me.MinimumTemperatureLine.Marks.Callout.Brush.Color = System.Drawing.Color.Black
        Me.MinimumTemperatureLine.Marks.Callout.Distance = 0
        Me.MinimumTemperatureLine.Marks.Callout.Draw3D = False
        Me.MinimumTemperatureLine.Marks.Callout.Length = 10
        Me.MinimumTemperatureLine.Marks.Callout.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle
        '
        '
        '
        '
        '
        '
        Me.MinimumTemperatureLine.Marks.Font.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        Me.MinimumTemperatureLine.Pointer.Brush.Color = System.Drawing.Color.Green
        Me.MinimumTemperatureLine.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle
        Me.MinimumTemperatureLine.Title = "Minimum temperature"
        '
        '
        '
        Me.MinimumTemperatureLine.XValues.DataMember = "X"
        Me.MinimumTemperatureLine.XValues.DateTime = True
        Me.MinimumTemperatureLine.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending
        '
        '
        '
        Me.MinimumTemperatureLine.YValues.DataMember = "Y"
        '
        'TabPage4
        '
        Me.TabPage4.Controls.Add(Me.RadiationChart)
        Me.TabPage4.ImageIndex = 3
        Me.TabPage4.Location = New System.Drawing.Point(4, 23)
        Me.TabPage4.Name = "TabPage4"
        Me.TabPage4.Size = New System.Drawing.Size(1007, 681)
        Me.TabPage4.TabIndex = 3
        Me.TabPage4.Text = "Radiation chart"
        Me.TabPage4.UseVisualStyleBackColor = True
        '
        'RadiationChart
        '
        '
        '
        '
        Me.RadiationChart.Aspect.ElevationFloat = 345
        Me.RadiationChart.Aspect.RotationFloat = 345
        Me.RadiationChart.Aspect.View3D = False
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Axes.Bottom.Automatic = True
        '
        '
        '
        Me.RadiationChart.Axes.Bottom.AxisPen.Width = 1
        '
        '
        '
        Me.RadiationChart.Axes.Bottom.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash
        Me.RadiationChart.Axes.Bottom.Grid.Visible = False
        Me.RadiationChart.Axes.Bottom.Grid.ZPosition = 0
        Me.RadiationChart.Axes.Bottom.Increment = 30
        '
        '
        '
        Me.RadiationChart.Axes.Bottom.Labels.DateTimeFormat = "MMM yyyy"
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Axes.Bottom.Labels.Font.Shadow.Visible = False
        Me.RadiationChart.Axes.Bottom.Labels.Font.Size = 11
        '
        '
        '
        Me.RadiationChart.Axes.Bottom.Labels.Shadow.Visible = False
        '
        '
        '
        Me.RadiationChart.Axes.Bottom.MinorTicks.Visible = False
        '
        '
        '
        Me.RadiationChart.Axes.Bottom.Ticks.Length = 0
        '
        '
        '
        Me.RadiationChart.Axes.Bottom.TicksInner.Length = 5
        '
        '
        '
        Me.RadiationChart.Axes.Bottom.Title.Caption = "Date"
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Axes.Bottom.Title.Font.Shadow.Visible = False
        Me.RadiationChart.Axes.Bottom.Title.Font.Size = 11
        Me.RadiationChart.Axes.Bottom.Title.Lines = New String() {"Date"}
        '
        '
        '
        Me.RadiationChart.Axes.Bottom.Title.Shadow.Visible = False
        '
        '
        '
        Me.RadiationChart.Axes.Depth.Automatic = True
        '
        '
        '
        Me.RadiationChart.Axes.Depth.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash
        Me.RadiationChart.Axes.Depth.Grid.ZPosition = 0
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Axes.Depth.Labels.Font.Shadow.Visible = False
        '
        '
        '
        Me.RadiationChart.Axes.Depth.Labels.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Axes.Depth.Title.Font.Shadow.Visible = False
        '
        '
        '
        Me.RadiationChart.Axes.Depth.Title.Shadow.Visible = False
        '
        '
        '
        Me.RadiationChart.Axes.DepthTop.Automatic = True
        '
        '
        '
        Me.RadiationChart.Axes.DepthTop.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash
        Me.RadiationChart.Axes.DepthTop.Grid.ZPosition = 0
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Axes.DepthTop.Labels.Font.Shadow.Visible = False
        '
        '
        '
        Me.RadiationChart.Axes.DepthTop.Labels.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Axes.DepthTop.Title.Font.Shadow.Visible = False
        '
        '
        '
        Me.RadiationChart.Axes.DepthTop.Title.Shadow.Visible = False
        '
        '
        '
        Me.RadiationChart.Axes.Left.Automatic = True
        '
        '
        '
        Me.RadiationChart.Axes.Left.AxisPen.Width = 1
        '
        '
        '
        Me.RadiationChart.Axes.Left.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash
        Me.RadiationChart.Axes.Left.Grid.Visible = False
        Me.RadiationChart.Axes.Left.Grid.ZPosition = 0
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Axes.Left.Labels.Font.Shadow.Visible = False
        Me.RadiationChart.Axes.Left.Labels.Font.Size = 11
        Me.RadiationChart.Axes.Left.Labels.Separation = 20
        '
        '
        '
        Me.RadiationChart.Axes.Left.Labels.Shadow.Visible = False
        '
        '
        '
        Me.RadiationChart.Axes.Left.MinorTicks.Visible = False
        '
        '
        '
        Me.RadiationChart.Axes.Left.Ticks.Length = 0
        '
        '
        '
        Me.RadiationChart.Axes.Left.TicksInner.Length = 5
        '
        '
        '
        Me.RadiationChart.Axes.Left.Title.Caption = "Rainfall (mm)"
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Axes.Left.Title.Font.Shadow.Visible = False
        Me.RadiationChart.Axes.Left.Title.Font.Size = 11
        Me.RadiationChart.Axes.Left.Title.Lines = New String() {"Rainfall (mm)"}
        '
        '
        '
        Me.RadiationChart.Axes.Left.Title.Shadow.Visible = False
        '
        '
        '
        Me.RadiationChart.Axes.Right.Automatic = True
        '
        '
        '
        Me.RadiationChart.Axes.Right.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash
        Me.RadiationChart.Axes.Right.Grid.Visible = False
        Me.RadiationChart.Axes.Right.Grid.ZPosition = 0
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Axes.Right.Labels.Font.Shadow.Visible = False
        Me.RadiationChart.Axes.Right.Labels.Font.Size = 11
        '
        '
        '
        Me.RadiationChart.Axes.Right.Labels.Shadow.Visible = False
        '
        '
        '
        Me.RadiationChart.Axes.Right.MinorTicks.Visible = False
        '
        '
        '
        Me.RadiationChart.Axes.Right.Ticks.Length = 0
        '
        '
        '
        Me.RadiationChart.Axes.Right.TicksInner.Length = 5
        '
        '
        '
        Me.RadiationChart.Axes.Right.Title.Caption = "Radiation (mJ/m2)"
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Axes.Right.Title.Font.Shadow.Visible = False
        Me.RadiationChart.Axes.Right.Title.Font.Size = 11
        Me.RadiationChart.Axes.Right.Title.Lines = New String() {"Radiation (mJ/m2)"}
        '
        '
        '
        Me.RadiationChart.Axes.Right.Title.Shadow.Visible = False
        '
        '
        '
        Me.RadiationChart.Axes.Top.Automatic = True
        '
        '
        '
        Me.RadiationChart.Axes.Top.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash
        Me.RadiationChart.Axes.Top.Grid.ZPosition = 0
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Axes.Top.Labels.Font.Shadow.Visible = False
        '
        '
        '
        Me.RadiationChart.Axes.Top.Labels.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Axes.Top.Title.Font.Shadow.Visible = False
        '
        '
        '
        Me.RadiationChart.Axes.Top.Title.Shadow.Visible = False
        Me.RadiationChart.Axes.Top.Visible = False
        Me.RadiationChart.BackColor = System.Drawing.Color.Transparent
        Me.RadiationChart.Cursor = System.Windows.Forms.Cursors.Default
        Me.RadiationChart.Dock = System.Windows.Forms.DockStyle.Fill
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Footer.Font.Shadow.Visible = False
        '
        '
        '
        Me.RadiationChart.Footer.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Header.Font.Shadow.Visible = False
        Me.RadiationChart.Header.Lines = New String() {""}
        '
        '
        '
        Me.RadiationChart.Header.Shadow.Visible = False
        '
        '
        '
        Me.RadiationChart.Legend.Alignment = Steema.TeeChart.LegendAlignments.Bottom
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Legend.Font.Shadow.Visible = False
        Me.RadiationChart.Legend.Font.Size = 11
        Me.RadiationChart.Legend.LegendStyle = Steema.TeeChart.LegendStyles.Series
        '
        '
        '
        Me.RadiationChart.Legend.Pen.Visible = False
        Me.RadiationChart.Legend.ResizeChart = False
        '
        '
        '
        Me.RadiationChart.Legend.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Legend.Title.Font.Bold = True
        '
        '
        '
        Me.RadiationChart.Legend.Title.Font.Shadow.Visible = False
        '
        '
        '
        Me.RadiationChart.Legend.Title.Pen.Visible = False
        '
        '
        '
        Me.RadiationChart.Legend.Title.Shadow.Visible = False
        Me.RadiationChart.Location = New System.Drawing.Point(0, 0)
        Me.RadiationChart.Name = "RadiationChart"
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Panel.Bevel.Outer = Steema.TeeChart.Drawing.BevelStyles.None
        '
        '
        '
        Me.RadiationChart.Panel.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(0, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        '
        '
        '
        Me.RadiationChart.Panel.ImageBevel.Width = 1
        '
        '
        '
        Me.RadiationChart.Panel.Shadow.Visible = False
        Me.RadiationChart.Series.Add(Me.RainfallBar2)
        Me.RadiationChart.Series.Add(Me.RadiationLine)
        Me.RadiationChart.Series.Add(Me.MaximumRadiationLine)
        Me.RadiationChart.Size = New System.Drawing.Size(1007, 681)
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.SubFooter.Font.Shadow.Visible = False
        '
        '
        '
        Me.RadiationChart.SubFooter.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.SubHeader.Font.Shadow.Visible = False
        '
        '
        '
        Me.RadiationChart.SubHeader.Shadow.Visible = False
        Me.RadiationChart.TabIndex = 16
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Walls.Back.AutoHide = False
        '
        '
        '
        Me.RadiationChart.Walls.Back.Shadow.Visible = False
        '
        '
        '
        Me.RadiationChart.Walls.Bottom.AutoHide = False
        '
        '
        '
        Me.RadiationChart.Walls.Bottom.Shadow.Visible = False
        '
        '
        '
        Me.RadiationChart.Walls.Left.AutoHide = False
        '
        '
        '
        Me.RadiationChart.Walls.Left.Shadow.Visible = False
        '
        '
        '
        Me.RadiationChart.Walls.Right.AutoHide = False
        '
        '
        '
        Me.RadiationChart.Walls.Right.Shadow.Visible = False
        Me.RadiationChart.Walls.Visible = False
        '
        'RainfallBar2
        '
        '
        '
        '
        Me.RainfallBar2.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(0, Byte), Integer), CType(CType(192, Byte), Integer), CType(CType(192, Byte), Integer))
        '
        '
        '
        '
        '
        '
        Me.RainfallBar2.Marks.Callout.ArrowHead = Steema.TeeChart.Styles.ArrowHeadStyles.None
        Me.RainfallBar2.Marks.Callout.ArrowHeadSize = 8
        '
        '
        '
        Me.RainfallBar2.Marks.Callout.Brush.Color = System.Drawing.Color.Black
        Me.RainfallBar2.Marks.Callout.Distance = 0
        Me.RainfallBar2.Marks.Callout.Draw3D = False
        Me.RainfallBar2.Marks.Callout.Length = 20
        Me.RainfallBar2.Marks.Callout.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle
        '
        '
        '
        '
        '
        '
        Me.RainfallBar2.Marks.Font.Shadow.Visible = False
        Me.RainfallBar2.Marks.Visible = False
        '
        '
        '
        Me.RainfallBar2.Pen.Color = System.Drawing.Color.FromArgb(CType(CType(0, Byte), Integer), CType(CType(115, Byte), Integer), CType(CType(115, Byte), Integer))
        Me.RainfallBar2.Title = "Rainfall"
        '
        '
        '
        Me.RainfallBar2.XValues.DataMember = "X"
        Me.RainfallBar2.XValues.DateTime = True
        Me.RainfallBar2.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending
        '
        '
        '
        Me.RainfallBar2.YValues.DataMember = "Bar"
        '
        'RadiationLine
        '
        '
        '
        '
        Me.RadiationLine.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(192, Byte), Integer), CType(CType(128, Byte), Integer))
        '
        '
        '
        Me.RadiationLine.LinePen.Color = System.Drawing.Color.FromArgb(CType(CType(153, Byte), Integer), CType(CType(115, Byte), Integer), CType(CType(77, Byte), Integer))
        '
        '
        '
        '
        '
        '
        Me.RadiationLine.Marks.Callout.ArrowHead = Steema.TeeChart.Styles.ArrowHeadStyles.None
        Me.RadiationLine.Marks.Callout.ArrowHeadSize = 8
        '
        '
        '
        Me.RadiationLine.Marks.Callout.Brush.Color = System.Drawing.Color.Black
        Me.RadiationLine.Marks.Callout.Distance = 0
        Me.RadiationLine.Marks.Callout.Draw3D = False
        Me.RadiationLine.Marks.Callout.Length = 10
        Me.RadiationLine.Marks.Callout.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle
        '
        '
        '
        '
        '
        '
        Me.RadiationLine.Marks.Font.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        Me.RadiationLine.Pointer.Brush.Color = System.Drawing.Color.Green
        Me.RadiationLine.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle
        Me.RadiationLine.Title = "Radiation"
        Me.RadiationLine.VertAxis = Steema.TeeChart.Styles.VerticalAxis.Right
        '
        '
        '
        Me.RadiationLine.XValues.DataMember = "X"
        Me.RadiationLine.XValues.DateTime = True
        Me.RadiationLine.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending
        '
        '
        '
        Me.RadiationLine.YValues.DataMember = "Y"
        '
        'MaximumRadiationLine
        '
        '
        '
        '
        Me.MaximumRadiationLine.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(0, Byte), Integer), CType(CType(0, Byte), Integer))
        '
        '
        '
        Me.MaximumRadiationLine.LinePen.Color = System.Drawing.Color.FromArgb(CType(CType(153, Byte), Integer), CType(CType(0, Byte), Integer), CType(CType(0, Byte), Integer))
        '
        '
        '
        '
        '
        '
        Me.MaximumRadiationLine.Marks.Callout.ArrowHead = Steema.TeeChart.Styles.ArrowHeadStyles.None
        Me.MaximumRadiationLine.Marks.Callout.ArrowHeadSize = 8
        '
        '
        '
        Me.MaximumRadiationLine.Marks.Callout.Brush.Color = System.Drawing.Color.Black
        Me.MaximumRadiationLine.Marks.Callout.Distance = 0
        Me.MaximumRadiationLine.Marks.Callout.Draw3D = False
        Me.MaximumRadiationLine.Marks.Callout.Length = 10
        Me.MaximumRadiationLine.Marks.Callout.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle
        '
        '
        '
        '
        '
        '
        Me.MaximumRadiationLine.Marks.Font.Shadow.Visible = False
        '
        '
        '
        '
        '
        '
        Me.MaximumRadiationLine.Pointer.Brush.Color = System.Drawing.Color.Green
        Me.MaximumRadiationLine.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle
        Me.MaximumRadiationLine.Title = "Maximum radiation"
        Me.MaximumRadiationLine.VertAxis = Steema.TeeChart.Styles.VerticalAxis.Right
        '
        '
        '
        Me.MaximumRadiationLine.XValues.DataMember = "X"
        Me.MaximumRadiationLine.XValues.DateTime = True
        Me.MaximumRadiationLine.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending
        '
        '
        '
        Me.MaximumRadiationLine.YValues.DataMember = "Y"
        '
        'MetGraphControl
        '
        Me.Controls.Add(Me.TabControl)
        Me.Name = "MetGraphControl"
        Me.Size = New System.Drawing.Size(1015, 724)
        Me.Controls.SetChildIndex(Me.MyHelpLabel, 0)
        Me.Controls.SetChildIndex(Me.TabControl, 0)
        Me.TabControl.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
        Me.TabPage1.PerformLayout()
        Me.YearPanel.ResumeLayout(False)
        Me.YearPanel.PerformLayout()
        CType(Me.YearStartBox, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.NumYearsBox, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TabPage2.ResumeLayout(False)
        Me.TabPage2.PerformLayout()
        Me.TabPage3.ResumeLayout(False)
        Me.TabPage5.ResumeLayout(False)
        Me.TabPage4.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

#End Region

    Public Overrides Sub OnRefresh()
        ContentsBox.Text = ""

        Dim FullFileName As String = Controller.ToAbsolute(FileName)
        If File.Exists(FullFileName) Then
            MetData = New DataTable()
            MetData.TableName = "Met"
            Metfile.ReadFromFile(FullFileName, MetData)
            StartDate = DataTableUtility.GetDateFromRow(MetData.Rows(0))
            EndDate = DataTableUtility.GetDateFromRow(MetData.Rows(MetData.Rows.Count - 1))
            PopulateRawData()
            RemoveHandler YearStartBox.ValueChanged, AddressOf YearStartBoxChanged
            RemoveHandler NumYearsBox.ValueChanged, AddressOf NumYearsBoxChanged
            YearStartBox.Value = StartDate.Year
            NumYearsBox.Value = 1
            AddHandler YearStartBox.ValueChanged, AddressOf YearStartBoxChanged
            AddHandler NumYearsBox.ValueChanged, AddressOf NumYearsBoxChanged
            RefreshAllCharts()
        End If
        YearPanel.Visible = (TabControl.SelectedIndex <> 0)
        YearPanel.Parent = Me
        YearPanel.BringToFront()
    End Sub

    Public Sub SetFileName(ByVal FileName As String)

        FileName = Controller.ToRelativePath(FileName)
        If Me.FileName <> FileName Then
            XmlHelper.SetValue(Data, "filename", FileName)
            Me.FileName = FileName
            OnRefresh()
        End If
    End Sub
    Public Function GetFileName() As String
        Return Me.FileName
    End Function
    Private Sub YearStartBoxChanged(ByVal sender As Object, ByVal e As System.EventArgs)
        RefreshAllCharts()
    End Sub
    Private Sub NumYearsBoxChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles NumYearsBox.ValueChanged
        RefreshAllCharts()
    End Sub
    Private Sub TabControl_TabIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TabControl.SelectedIndexChanged
        YearPanel.Visible = (TabControl.SelectedIndex <> 0)
    End Sub

    Private Sub PopulateRawData()
        Dim sr As StreamReader = New StreamReader(Controller.ToAbsolute(FileName))
        ContentsBox.Text = sr.ReadToEnd
        sr.Close()
    End Sub

    Private Sub RefreshAllCharts()
        ' ----------------------------------------------------------------------------------
        ' Refresh all data for current year and attach data to lines and bars on chart.
        ' ----------------------------------------------------------------------------------

        If MetData.Rows.Count > 0 Then
            YearlyData = DataTableUtility.FilterTableForYear(MetData, YearStartBox.Value, YearStartBox.Value + NumYearsBox.Value - 1)

            If YearlyData.Table.Columns.IndexOf("Rain") <> -1 Then
                Dim Rainfall As Double() = DataTableUtility.ColumnValues(YearlyData, "rain")
                If NumYearsBox.Value = 1 Then
                    RainfallLabel.Text = MathUtility.Sum(Rainfall).ToString("f1") + " mm for the year " + YearStartBox.Value.ToString
                Else
                    RainfallLabel.Text = MathUtility.Sum(Rainfall).ToString("f1") + " mm for the years " + YearStartBox.Value.ToString + " to " + (YearStartBox.Value + NumYearsBox.Value - 1).ToString
                End If

            Else
                RainfallLabel.Text = ""
            End If
            MonthlyData = DataTableUtility.MonthlySums(YearlyData)
            CalcQmax()
            PopulateSeries(RainfallBar, YearlyData, "Rain")
            PopulateSeries(RainfallBar2, YearlyData, "Rain")
            PopulateSeries(MaximumTemperatureLine, YearlyData, "MaxT")
            PopulateSeries(MinimumTemperatureLine, YearlyData, "MinT")
            PopulateSeries(RadiationLine, YearlyData, "Radn")
            PopulateSeries(MaximumRadiationLine, YearlyData, "QMax")
            PopulateSeries(MonthlyRainfallBar, MonthlyData, "Rain")
            If MonthlyData.Columns.IndexOf("pan") <> -1 Then
                PopulateSeries(MonthlyEvaporationLine, MonthlyData, "pan")
            Else
                PopulateSeries(MonthlyEvaporationLine, MonthlyData, "Evap")
            End If
        End If
    End Sub


    Private Sub PopulateSeries(ByVal RainfallBar As Series, ByVal Data As DataView, ByVal ColumnName As String)
        RainfallBar.Clear()
        If Data.Table.Columns.IndexOf(ColumnName) <> -1 Then
            For Row As Integer = 0 To Data.Count - 1
                Dim D As DateTime = DataTableUtility.GetDateFromRow(Data(Row).Row)
                RainfallBar.Add(D, Convert.ToDouble(Data(Row)(ColumnName)))
            Next
        End If
    End Sub

    Private Sub PopulateSeries(ByVal RainfallBar As Series, ByVal Data As DataTable, ByVal ColumnName As String)
        RainfallBar.Clear()
        If Data.Columns.IndexOf(ColumnName) <> -1 AndAlso Data.Rows.Count > 0 AndAlso Not Convert.IsDBNull(Data.Rows(0)(ColumnName)) Then
            For Row As Integer = 0 To Data.Rows.Count - 1
                Dim D As DateTime = DataTableUtility.GetDateFromRow(Data.Rows(Row))
                RainfallBar.Add(D, Convert.ToDouble(Data.Rows(Row)(ColumnName)))
            Next
        End If
    End Sub


    Private Sub CalcQmax()
        ' ----------------------------------------------------------------------------------
        ' Add a calculated QMax column to the daily data.
        ' ----------------------------------------------------------------------------------
        If (IsNothing(MetData.Columns("Qmax"))) Then
            MetData.Columns.Add("Qmax")
        End If

        ' Do we have a VP column?
        Dim HaveVPColumn As Boolean = Not IsNothing(MetData.Columns("VP"))

        ' Get latitude for later on.
        Dim Latitude As Single = Metfile.Constant("latitude").Value

        ' Loop through all rows and calculate a QMax
        Dim doy As Integer = 0
        For Row As Integer = 0 To YearlyData.Count - 1
            doy = doy + 1
            If HaveVPColumn AndAlso Not Convert.IsDBNull(YearlyData(Row)("vp")) Then
                YearlyData(Row)("Qmax") = QMax(doy + 1, Latitude, VBMet.Taz, VBMet.Alpha, YearlyData(Row)("vp"))
            Else
                YearlyData(Row)("Qmax") = QMax(doy + 1, Latitude, VBMet.Taz, VBMet.Alpha, svp(YearlyData(Row)("mint")))
            End If
        Next

    End Sub



End Class
