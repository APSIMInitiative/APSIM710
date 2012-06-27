
Imports System.Math
Imports System.IO
Imports System.Drawing
Imports System.Xml

Imports ApsimFile
Imports Controllers
Imports CSGeneral
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
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(MetGraphControl))
        Me.TabImages = New System.Windows.Forms.ImageList(Me.components)
        Me.TabControl = New System.Windows.Forms.TabControl()
        Me.TabPage1 = New System.Windows.Forms.TabPage()
        Me.YearPanel = New System.Windows.Forms.Panel()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.YearStartBox = New System.Windows.Forms.NumericUpDown()
        Me.NumYearsBox = New System.Windows.Forms.NumericUpDown()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.ContentsBox = New System.Windows.Forms.TextBox()
        Me.TabPage2 = New System.Windows.Forms.TabPage()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.RainfallLabel = New System.Windows.Forms.Label()
        Me.RainfallChart = New Steema.TeeChart.TChart()
        Me.RainfallBar = New Steema.TeeChart.Styles.Bar()
        Me.TabPage3 = New System.Windows.Forms.TabPage()
        Me.MonthlyRainfallChart = New Steema.TeeChart.TChart()
        Me.MonthlyRainfallBar = New Steema.TeeChart.Styles.Bar()
        Me.MonthlyEvaporationLine = New Steema.TeeChart.Styles.Line()
        Me.TabPage5 = New System.Windows.Forms.TabPage()
        Me.TemperatureChart = New Steema.TeeChart.TChart()
        Me.MaximumTemperatureLine = New Steema.TeeChart.Styles.Line()
        Me.MinimumTemperatureLine = New Steema.TeeChart.Styles.Line()
        Me.TabPage4 = New System.Windows.Forms.TabPage()
        Me.RadiationChart = New Steema.TeeChart.TChart()
        Me.RainfallBar2 = New Steema.TeeChart.Styles.Bar()
        Me.RadiationLine = New Steema.TeeChart.Styles.Line()
        Me.MaximumRadiationLine = New Steema.TeeChart.Styles.Line()
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
        Me.TabPage1.Location = New System.Drawing.Point(4, 25)
        Me.TabPage1.Name = "TabPage1"
        Me.TabPage1.Padding = New System.Windows.Forms.Padding(3)
        Me.TabPage1.Size = New System.Drawing.Size(1007, 679)
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
        Me.Label5.Size = New System.Drawing.Size(42, 17)
        Me.Label5.TabIndex = 20
        Me.Label5.Text = "Show"
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.Location = New System.Drawing.Point(3, 7)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(42, 17)
        Me.Label4.TabIndex = 19
        Me.Label4.Text = "Year:"
        '
        'YearStartBox
        '
        Me.YearStartBox.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.YearStartBox.CausesValidation = False
        Me.YearStartBox.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.YearStartBox.Location = New System.Drawing.Point(41, 5)
        Me.YearStartBox.Maximum = New Decimal(New Integer() {2200, 0, 0, 0})
        Me.YearStartBox.Minimum = New Decimal(New Integer() {1800, 0, 0, 0})
        Me.YearStartBox.Name = "YearStartBox"
        Me.YearStartBox.Size = New System.Drawing.Size(64, 24)
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
        Me.NumYearsBox.Size = New System.Drawing.Size(49, 24)
        Me.NumYearsBox.TabIndex = 17
        Me.NumYearsBox.Value = New Decimal(New Integer() {1, 0, 0, 0})
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(96, 36)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(101, 17)
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
        Me.ContentsBox.Size = New System.Drawing.Size(1001, 673)
        Me.ContentsBox.TabIndex = 0
        Me.ContentsBox.WordWrap = False
        '
        'TabPage2
        '
        Me.TabPage2.Controls.Add(Me.Label1)
        Me.TabPage2.Controls.Add(Me.RainfallLabel)
        Me.TabPage2.Controls.Add(Me.RainfallChart)
        Me.TabPage2.ImageIndex = 1
        Me.TabPage2.Location = New System.Drawing.Point(4, 25)
        Me.TabPage2.Name = "TabPage2"
        Me.TabPage2.Padding = New System.Windows.Forms.Padding(3)
        Me.TabPage2.Size = New System.Drawing.Size(1007, 679)
        Me.TabPage2.TabIndex = 1
        Me.TabPage2.Text = "Rainfall chart"
        Me.TabPage2.UseVisualStyleBackColor = True
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(303, 11)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(95, 17)
        Me.Label1.TabIndex = 16
        Me.Label1.Text = "Total Rainfall:"
        '
        'RainfallLabel
        '
        Me.RainfallLabel.AutoSize = True
        Me.RainfallLabel.Location = New System.Drawing.Point(381, 11)
        Me.RainfallLabel.Name = "RainfallLabel"
        Me.RainfallLabel.Size = New System.Drawing.Size(24, 17)
        Me.RainfallLabel.TabIndex = 15
        Me.RainfallLabel.Text = "L1"
        '
        'RainfallChart
        '
        '
        '
        '
        Me.RainfallChart.Aspect.View3D = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Axes.Bottom.AxisPen.Width = 1
        '
        '
        '
        Me.RainfallChart.Axes.Bottom.Grid.Visible = False
        Me.RainfallChart.Axes.Bottom.Increment = 30.0R
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Axes.Bottom.Labels.Bevel.StringColorOne = "FFFFFFFF"
        Me.RainfallChart.Axes.Bottom.Labels.Bevel.StringColorTwo = "FF808080"
        Me.RainfallChart.Axes.Bottom.Labels.CustomSize = 40
        Me.RainfallChart.Axes.Bottom.Labels.DateTimeFormat = "MMM yyyy"
        '
        '
        '
        Me.RainfallChart.Axes.Bottom.Labels.Font.Size = 11
        Me.RainfallChart.Axes.Bottom.Labels.Font.SizeFloat = 11.0!
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
        '
        '
        '
        Me.RainfallChart.Axes.Bottom.Title.Bevel.StringColorOne = "FFFFFFFF"
        Me.RainfallChart.Axes.Bottom.Title.Bevel.StringColorTwo = "FF808080"
        Me.RainfallChart.Axes.Bottom.Title.Caption = "Date"
        '
        '
        '
        Me.RainfallChart.Axes.Bottom.Title.Font.Size = 11
        Me.RainfallChart.Axes.Bottom.Title.Font.SizeFloat = 11.0!
        Me.RainfallChart.Axes.Bottom.Title.Lines = New String() {"Date"}
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Axes.Depth.Labels.Bevel.StringColorOne = "FFFFFFFF"
        Me.RainfallChart.Axes.Depth.Labels.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Axes.Depth.Title.Bevel.StringColorOne = "FFFFFFFF"
        Me.RainfallChart.Axes.Depth.Title.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Axes.DepthTop.Labels.Bevel.StringColorOne = "FFFFFFFF"
        Me.RainfallChart.Axes.DepthTop.Labels.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Axes.DepthTop.Title.Bevel.StringColorOne = "FFFFFFFF"
        Me.RainfallChart.Axes.DepthTop.Title.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Axes.Left.AxisPen.Width = 1
        '
        '
        '
        Me.RainfallChart.Axes.Left.Grid.Visible = False
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Axes.Left.Labels.Bevel.StringColorOne = "FFFFFFFF"
        Me.RainfallChart.Axes.Left.Labels.Bevel.StringColorTwo = "FF808080"
        Me.RainfallChart.Axes.Left.Labels.CustomSize = 40
        '
        '
        '
        Me.RainfallChart.Axes.Left.Labels.Font.Size = 11
        Me.RainfallChart.Axes.Left.Labels.Font.SizeFloat = 11.0!
        Me.RainfallChart.Axes.Left.Labels.Separation = 20
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
        '
        '
        '
        Me.RainfallChart.Axes.Left.Title.Bevel.StringColorOne = "FFFFFFFF"
        Me.RainfallChart.Axes.Left.Title.Bevel.StringColorTwo = "FF808080"
        Me.RainfallChart.Axes.Left.Title.Caption = "Rainfall (mm)"
        '
        '
        '
        Me.RainfallChart.Axes.Left.Title.Font.Size = 11
        Me.RainfallChart.Axes.Left.Title.Font.SizeFloat = 11.0!
        Me.RainfallChart.Axes.Left.Title.Lines = New String() {"Rainfall (mm)"}
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Axes.Right.Labels.Bevel.StringColorOne = "FFFFFFFF"
        Me.RainfallChart.Axes.Right.Labels.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Axes.Right.Title.Bevel.StringColorOne = "FFFFFFFF"
        Me.RainfallChart.Axes.Right.Title.Bevel.StringColorTwo = "FF808080"
        Me.RainfallChart.Axes.Right.Visible = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Axes.Top.Labels.Bevel.StringColorOne = "FFFFFFFF"
        Me.RainfallChart.Axes.Top.Labels.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Axes.Top.Title.Bevel.StringColorOne = "FFFFFFFF"
        Me.RainfallChart.Axes.Top.Title.Bevel.StringColorTwo = "FF808080"
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
        Me.RainfallChart.Footer.Bevel.StringColorOne = "FFFFFFFF"
        Me.RainfallChart.Footer.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Header.Bevel.StringColorOne = "FFFFFFFF"
        Me.RainfallChart.Header.Bevel.StringColorTwo = "FF808080"
        Me.RainfallChart.Header.Lines = New String() {""}
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Legend.Bevel.StringColorOne = "FFFFFFFF"
        Me.RainfallChart.Legend.Bevel.StringColorTwo = "FF808080"
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
        Me.RainfallChart.Legend.Title.Bevel.StringColorOne = "FFFFFFFF"
        Me.RainfallChart.Legend.Title.Bevel.StringColorTwo = "FF808080"
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
        Me.RainfallChart.Panel.Bevel.StringColorOne = "FFFFFFFF"
        Me.RainfallChart.Panel.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        Me.RainfallChart.Panel.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(0, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        '
        '
        '
        Me.RainfallChart.Panel.ImageBevel.Width = 1
        Me.RainfallChart.Panel.MarginBottom = 1.0R
        Me.RainfallChart.Panel.MarginLeft = 1.0R
        Me.RainfallChart.Series.Add(Me.RainfallBar)
        Me.RainfallChart.Size = New System.Drawing.Size(1001, 673)
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.SubFooter.Bevel.StringColorOne = "FFFFFFFF"
        Me.RainfallChart.SubFooter.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.SubHeader.Bevel.StringColorOne = "FFFFFFFF"
        Me.RainfallChart.SubHeader.Bevel.StringColorTwo = "FF808080"
        Me.RainfallChart.TabIndex = 14
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Walls.Back.Bevel.StringColorOne = "FFFFFFFF"
        Me.RainfallChart.Walls.Back.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Walls.Bottom.Bevel.StringColorOne = "FFFFFFFF"
        Me.RainfallChart.Walls.Bottom.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Walls.Left.Bevel.StringColorOne = "FFFFFFFF"
        Me.RainfallChart.Walls.Left.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.RainfallChart.Walls.Right.Bevel.StringColorOne = "FFFFFFFF"
        Me.RainfallChart.Walls.Right.Bevel.StringColorTwo = "FF808080"
        Me.RainfallChart.Walls.Visible = False
        '
        'RainfallBar
        '
        '
        '
        '
        Me.RainfallBar.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(0, Byte), Integer), CType(CType(192, Byte), Integer), CType(CType(192, Byte), Integer))
        Me.RainfallBar.Color = System.Drawing.Color.FromArgb(CType(CType(0, Byte), Integer), CType(CType(192, Byte), Integer), CType(CType(192, Byte), Integer))
        Me.RainfallBar.ColorEach = False
        '
        '
        '
        '
        '
        '
        Me.RainfallBar.Marks.Bevel.StringColorOne = "FFFFFFFF"
        Me.RainfallBar.Marks.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.RainfallBar.Marks.Symbol.Bevel.StringColorOne = "FFFFFFFF"
        Me.RainfallBar.Marks.Symbol.Bevel.StringColorTwo = "FF808080"
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
        Me.TabPage3.Location = New System.Drawing.Point(4, 25)
        Me.TabPage3.Name = "TabPage3"
        Me.TabPage3.Padding = New System.Windows.Forms.Padding(3)
        Me.TabPage3.Size = New System.Drawing.Size(1007, 679)
        Me.TabPage3.TabIndex = 2
        Me.TabPage3.Text = "Monthly rainfall chart"
        Me.TabPage3.UseVisualStyleBackColor = True
        '
        'MonthlyRainfallChart
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Aspect.View3D = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Bottom.AxisPen.Width = 1
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Bottom.Grid.Visible = False
        Me.MonthlyRainfallChart.Axes.Bottom.Increment = 30.0R
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Bottom.Labels.Bevel.StringColorOne = "FFFFFFFF"
        Me.MonthlyRainfallChart.Axes.Bottom.Labels.Bevel.StringColorTwo = "FF808080"
        Me.MonthlyRainfallChart.Axes.Bottom.Labels.CustomSize = 40
        Me.MonthlyRainfallChart.Axes.Bottom.Labels.DateTimeFormat = "MMM yyyy"
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Bottom.Labels.Font.Size = 11
        Me.MonthlyRainfallChart.Axes.Bottom.Labels.Font.SizeFloat = 11.0!
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
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Bottom.Title.Bevel.StringColorOne = "FFFFFFFF"
        Me.MonthlyRainfallChart.Axes.Bottom.Title.Bevel.StringColorTwo = "FF808080"
        Me.MonthlyRainfallChart.Axes.Bottom.Title.Caption = "Date"
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Bottom.Title.Font.Size = 11
        Me.MonthlyRainfallChart.Axes.Bottom.Title.Font.SizeFloat = 11.0!
        Me.MonthlyRainfallChart.Axes.Bottom.Title.Lines = New String() {"Date"}
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Depth.Labels.Bevel.StringColorOne = "FFFFFFFF"
        Me.MonthlyRainfallChart.Axes.Depth.Labels.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Depth.Title.Bevel.StringColorOne = "FFFFFFFF"
        Me.MonthlyRainfallChart.Axes.Depth.Title.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.DepthTop.Labels.Bevel.StringColorOne = "FFFFFFFF"
        Me.MonthlyRainfallChart.Axes.DepthTop.Labels.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.DepthTop.Title.Bevel.StringColorOne = "FFFFFFFF"
        Me.MonthlyRainfallChart.Axes.DepthTop.Title.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Left.AxisPen.Width = 1
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Left.Grid.Visible = False
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Left.Labels.Bevel.StringColorOne = "FFFFFFFF"
        Me.MonthlyRainfallChart.Axes.Left.Labels.Bevel.StringColorTwo = "FF808080"
        Me.MonthlyRainfallChart.Axes.Left.Labels.CustomSize = 40
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Left.Labels.Font.Size = 11
        Me.MonthlyRainfallChart.Axes.Left.Labels.Font.SizeFloat = 11.0!
        Me.MonthlyRainfallChart.Axes.Left.Labels.Separation = 20
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
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Left.Title.Bevel.StringColorOne = "FFFFFFFF"
        Me.MonthlyRainfallChart.Axes.Left.Title.Bevel.StringColorTwo = "FF808080"
        Me.MonthlyRainfallChart.Axes.Left.Title.Caption = "Rainfall and Evaporation (mm)"
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Left.Title.Font.Size = 11
        Me.MonthlyRainfallChart.Axes.Left.Title.Font.SizeFloat = 11.0!
        Me.MonthlyRainfallChart.Axes.Left.Title.Lines = New String() {"Rainfall and Evaporation (mm)"}
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Right.Labels.Bevel.StringColorOne = "FFFFFFFF"
        Me.MonthlyRainfallChart.Axes.Right.Labels.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Right.Title.Bevel.StringColorOne = "FFFFFFFF"
        Me.MonthlyRainfallChart.Axes.Right.Title.Bevel.StringColorTwo = "FF808080"
        Me.MonthlyRainfallChart.Axes.Right.Visible = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Top.Labels.Bevel.StringColorOne = "FFFFFFFF"
        Me.MonthlyRainfallChart.Axes.Top.Labels.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Axes.Top.Title.Bevel.StringColorOne = "FFFFFFFF"
        Me.MonthlyRainfallChart.Axes.Top.Title.Bevel.StringColorTwo = "FF808080"
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
        Me.MonthlyRainfallChart.Footer.Bevel.StringColorOne = "FFFFFFFF"
        Me.MonthlyRainfallChart.Footer.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Header.Bevel.StringColorOne = "FFFFFFFF"
        Me.MonthlyRainfallChart.Header.Bevel.StringColorTwo = "FF808080"
        Me.MonthlyRainfallChart.Header.Lines = New String() {""}
        '
        '
        '
        Me.MonthlyRainfallChart.Legend.Alignment = Steema.TeeChart.LegendAlignments.Bottom
        '
        '
        '
        Me.MonthlyRainfallChart.Legend.Bevel.StringColorOne = "FFFFFFFF"
        Me.MonthlyRainfallChart.Legend.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        Me.MonthlyRainfallChart.Legend.Font.Size = 11
        Me.MonthlyRainfallChart.Legend.Font.SizeFloat = 11.0!
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
        Me.MonthlyRainfallChart.Legend.Title.Bevel.StringColorOne = "FFFFFFFF"
        Me.MonthlyRainfallChart.Legend.Title.Bevel.StringColorTwo = "FF808080"
        Me.MonthlyRainfallChart.Location = New System.Drawing.Point(3, 3)
        Me.MonthlyRainfallChart.Name = "MonthlyRainfallChart"
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Panel.Bevel.Outer = Steema.TeeChart.Drawing.BevelStyles.None
        Me.MonthlyRainfallChart.Panel.Bevel.StringColorOne = "FFFFFFFF"
        Me.MonthlyRainfallChart.Panel.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        Me.MonthlyRainfallChart.Panel.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(0, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        '
        '
        '
        Me.MonthlyRainfallChart.Panel.ImageBevel.Width = 1
        Me.MonthlyRainfallChart.Panel.MarginBottom = 1.0R
        Me.MonthlyRainfallChart.Panel.MarginLeft = 1.0R
        Me.MonthlyRainfallChart.Series.Add(Me.MonthlyRainfallBar)
        Me.MonthlyRainfallChart.Series.Add(Me.MonthlyEvaporationLine)
        Me.MonthlyRainfallChart.Size = New System.Drawing.Size(1001, 673)
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.SubFooter.Bevel.StringColorOne = "FFFFFFFF"
        Me.MonthlyRainfallChart.SubFooter.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.SubHeader.Bevel.StringColorOne = "FFFFFFFF"
        Me.MonthlyRainfallChart.SubHeader.Bevel.StringColorTwo = "FF808080"
        Me.MonthlyRainfallChart.TabIndex = 15
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Walls.Back.Bevel.StringColorOne = "FFFFFFFF"
        Me.MonthlyRainfallChart.Walls.Back.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Walls.Bottom.Bevel.StringColorOne = "FFFFFFFF"
        Me.MonthlyRainfallChart.Walls.Bottom.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Walls.Left.Bevel.StringColorOne = "FFFFFFFF"
        Me.MonthlyRainfallChart.Walls.Left.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallChart.Walls.Right.Bevel.StringColorOne = "FFFFFFFF"
        Me.MonthlyRainfallChart.Walls.Right.Bevel.StringColorTwo = "FF808080"
        Me.MonthlyRainfallChart.Walls.Visible = False
        '
        'MonthlyRainfallBar
        '
        '
        '
        '
        Me.MonthlyRainfallBar.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(0, Byte), Integer), CType(CType(192, Byte), Integer), CType(CType(192, Byte), Integer))
        Me.MonthlyRainfallBar.Color = System.Drawing.Color.FromArgb(CType(CType(0, Byte), Integer), CType(CType(192, Byte), Integer), CType(CType(192, Byte), Integer))
        Me.MonthlyRainfallBar.ColorEach = False
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallBar.Marks.Bevel.StringColorOne = "FFFFFFFF"
        Me.MonthlyRainfallBar.Marks.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.MonthlyRainfallBar.Marks.Symbol.Bevel.StringColorOne = "FFFFFFFF"
        Me.MonthlyRainfallBar.Marks.Symbol.Bevel.StringColorTwo = "FF808080"
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
        Me.MonthlyEvaporationLine.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(68, Byte), Integer), CType(CType(102, Byte), Integer), CType(CType(163, Byte), Integer))
        Me.MonthlyEvaporationLine.Color = System.Drawing.Color.FromArgb(CType(CType(68, Byte), Integer), CType(CType(102, Byte), Integer), CType(CType(163, Byte), Integer))
        Me.MonthlyEvaporationLine.ColorEach = False
        '
        '
        '
        Me.MonthlyEvaporationLine.LinePen.Color = System.Drawing.Color.FromArgb(CType(CType(41, Byte), Integer), CType(CType(61, Byte), Integer), CType(CType(98, Byte), Integer))
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
        Me.TabPage5.Location = New System.Drawing.Point(4, 25)
        Me.TabPage5.Name = "TabPage5"
        Me.TabPage5.Size = New System.Drawing.Size(1007, 679)
        Me.TabPage5.TabIndex = 4
        Me.TabPage5.Text = "Temperature chart"
        Me.TabPage5.UseVisualStyleBackColor = True
        '
        'TemperatureChart
        '
        '
        '
        '
        Me.TemperatureChart.Aspect.View3D = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Axes.Bottom.AxisPen.Width = 1
        '
        '
        '
        Me.TemperatureChart.Axes.Bottom.Grid.Visible = False
        Me.TemperatureChart.Axes.Bottom.Increment = 30.0R
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Axes.Bottom.Labels.Bevel.StringColorOne = "FFFFFFFF"
        Me.TemperatureChart.Axes.Bottom.Labels.Bevel.StringColorTwo = "FF808080"
        Me.TemperatureChart.Axes.Bottom.Labels.DateTimeFormat = "MMM yyyy"
        '
        '
        '
        Me.TemperatureChart.Axes.Bottom.Labels.Font.Size = 11
        Me.TemperatureChart.Axes.Bottom.Labels.Font.SizeFloat = 11.0!
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
        '
        '
        '
        Me.TemperatureChart.Axes.Bottom.Title.Bevel.StringColorOne = "FFFFFFFF"
        Me.TemperatureChart.Axes.Bottom.Title.Bevel.StringColorTwo = "FF808080"
        Me.TemperatureChart.Axes.Bottom.Title.Caption = "Date"
        '
        '
        '
        Me.TemperatureChart.Axes.Bottom.Title.Font.Size = 11
        Me.TemperatureChart.Axes.Bottom.Title.Font.SizeFloat = 11.0!
        Me.TemperatureChart.Axes.Bottom.Title.Lines = New String() {"Date"}
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Axes.Depth.Labels.Bevel.StringColorOne = "FFFFFFFF"
        Me.TemperatureChart.Axes.Depth.Labels.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Axes.Depth.Title.Bevel.StringColorOne = "FFFFFFFF"
        Me.TemperatureChart.Axes.Depth.Title.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Axes.DepthTop.Labels.Bevel.StringColorOne = "FFFFFFFF"
        Me.TemperatureChart.Axes.DepthTop.Labels.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Axes.DepthTop.Title.Bevel.StringColorOne = "FFFFFFFF"
        Me.TemperatureChart.Axes.DepthTop.Title.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Axes.Left.AxisPen.Width = 1
        '
        '
        '
        Me.TemperatureChart.Axes.Left.Grid.Visible = False
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Axes.Left.Labels.Bevel.StringColorOne = "FFFFFFFF"
        Me.TemperatureChart.Axes.Left.Labels.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        Me.TemperatureChart.Axes.Left.Labels.Font.Size = 11
        Me.TemperatureChart.Axes.Left.Labels.Font.SizeFloat = 11.0!
        Me.TemperatureChart.Axes.Left.Labels.Separation = 20
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
        '
        '
        '
        Me.TemperatureChart.Axes.Left.Title.Bevel.StringColorOne = "FFFFFFFF"
        Me.TemperatureChart.Axes.Left.Title.Bevel.StringColorTwo = "FF808080"
        Me.TemperatureChart.Axes.Left.Title.Caption = "Temperature (oC)"
        '
        '
        '
        Me.TemperatureChart.Axes.Left.Title.Font.Size = 11
        Me.TemperatureChart.Axes.Left.Title.Font.SizeFloat = 11.0!
        Me.TemperatureChart.Axes.Left.Title.Lines = New String() {"Temperature (oC)"}
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Axes.Right.Labels.Bevel.StringColorOne = "FFFFFFFF"
        Me.TemperatureChart.Axes.Right.Labels.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Axes.Right.Title.Bevel.StringColorOne = "FFFFFFFF"
        Me.TemperatureChart.Axes.Right.Title.Bevel.StringColorTwo = "FF808080"
        Me.TemperatureChart.Axes.Right.Visible = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Axes.Top.Labels.Bevel.StringColorOne = "FFFFFFFF"
        Me.TemperatureChart.Axes.Top.Labels.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Axes.Top.Title.Bevel.StringColorOne = "FFFFFFFF"
        Me.TemperatureChart.Axes.Top.Title.Bevel.StringColorTwo = "FF808080"
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
        Me.TemperatureChart.Footer.Bevel.StringColorOne = "FFFFFFFF"
        Me.TemperatureChart.Footer.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Header.Bevel.StringColorOne = "FFFFFFFF"
        Me.TemperatureChart.Header.Bevel.StringColorTwo = "FF808080"
        Me.TemperatureChart.Header.Lines = New String() {""}
        '
        '
        '
        Me.TemperatureChart.Legend.Alignment = Steema.TeeChart.LegendAlignments.Bottom
        '
        '
        '
        Me.TemperatureChart.Legend.Bevel.StringColorOne = "FFFFFFFF"
        Me.TemperatureChart.Legend.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        Me.TemperatureChart.Legend.Font.Size = 11
        Me.TemperatureChart.Legend.Font.SizeFloat = 11.0!
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
        Me.TemperatureChart.Legend.Title.Bevel.StringColorOne = "FFFFFFFF"
        Me.TemperatureChart.Legend.Title.Bevel.StringColorTwo = "FF808080"
        Me.TemperatureChart.Location = New System.Drawing.Point(0, 0)
        Me.TemperatureChart.Name = "TemperatureChart"
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Panel.Bevel.Outer = Steema.TeeChart.Drawing.BevelStyles.None
        Me.TemperatureChart.Panel.Bevel.StringColorOne = "FFFFFFFF"
        Me.TemperatureChart.Panel.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        Me.TemperatureChart.Panel.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(0, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        '
        '
        '
        Me.TemperatureChart.Panel.ImageBevel.Width = 1
        Me.TemperatureChart.Series.Add(Me.MaximumTemperatureLine)
        Me.TemperatureChart.Series.Add(Me.MinimumTemperatureLine)
        Me.TemperatureChart.Size = New System.Drawing.Size(1007, 679)
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.SubFooter.Bevel.StringColorOne = "FFFFFFFF"
        Me.TemperatureChart.SubFooter.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.SubHeader.Bevel.StringColorOne = "FFFFFFFF"
        Me.TemperatureChart.SubHeader.Bevel.StringColorTwo = "FF808080"
        Me.TemperatureChart.TabIndex = 16
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Walls.Back.Bevel.StringColorOne = "FFFFFFFF"
        Me.TemperatureChart.Walls.Back.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Walls.Bottom.Bevel.StringColorOne = "FFFFFFFF"
        Me.TemperatureChart.Walls.Bottom.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Walls.Left.Bevel.StringColorOne = "FFFFFFFF"
        Me.TemperatureChart.Walls.Left.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.TemperatureChart.Walls.Right.Bevel.StringColorOne = "FFFFFFFF"
        Me.TemperatureChart.Walls.Right.Bevel.StringColorTwo = "FF808080"
        Me.TemperatureChart.Walls.Visible = False
        '
        'MaximumTemperatureLine
        '
        '
        '
        '
        Me.MaximumTemperatureLine.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(68, Byte), Integer), CType(CType(102, Byte), Integer), CType(CType(163, Byte), Integer))
        Me.MaximumTemperatureLine.Color = System.Drawing.Color.FromArgb(CType(CType(68, Byte), Integer), CType(CType(102, Byte), Integer), CType(CType(163, Byte), Integer))
        Me.MaximumTemperatureLine.ColorEach = False
        '
        '
        '
        Me.MaximumTemperatureLine.LinePen.Color = System.Drawing.Color.FromArgb(CType(CType(41, Byte), Integer), CType(CType(61, Byte), Integer), CType(CType(98, Byte), Integer))
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
        Me.MinimumTemperatureLine.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(243, Byte), Integer), CType(CType(156, Byte), Integer), CType(CType(53, Byte), Integer))
        Me.MinimumTemperatureLine.Color = System.Drawing.Color.FromArgb(CType(CType(243, Byte), Integer), CType(CType(156, Byte), Integer), CType(CType(53, Byte), Integer))
        Me.MinimumTemperatureLine.ColorEach = False
        '
        '
        '
        Me.MinimumTemperatureLine.LinePen.Color = System.Drawing.Color.FromArgb(CType(CType(146, Byte), Integer), CType(CType(94, Byte), Integer), CType(CType(32, Byte), Integer))
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
        Me.TabPage4.Location = New System.Drawing.Point(4, 25)
        Me.TabPage4.Name = "TabPage4"
        Me.TabPage4.Size = New System.Drawing.Size(1007, 679)
        Me.TabPage4.TabIndex = 3
        Me.TabPage4.Text = "Radiation chart"
        Me.TabPage4.UseVisualStyleBackColor = True
        '
        'RadiationChart
        '
        '
        '
        '
        Me.RadiationChart.Aspect.View3D = False
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Axes.Bottom.AxisPen.Width = 1
        '
        '
        '
        Me.RadiationChart.Axes.Bottom.Grid.Visible = False
        Me.RadiationChart.Axes.Bottom.Increment = 30.0R
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Axes.Bottom.Labels.Bevel.StringColorOne = "FFFFFFFF"
        Me.RadiationChart.Axes.Bottom.Labels.Bevel.StringColorTwo = "FF808080"
        Me.RadiationChart.Axes.Bottom.Labels.DateTimeFormat = "MMM yyyy"
        '
        '
        '
        Me.RadiationChart.Axes.Bottom.Labels.Font.Size = 11
        Me.RadiationChart.Axes.Bottom.Labels.Font.SizeFloat = 11.0!
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
        '
        '
        '
        Me.RadiationChart.Axes.Bottom.Title.Bevel.StringColorOne = "FFFFFFFF"
        Me.RadiationChart.Axes.Bottom.Title.Bevel.StringColorTwo = "FF808080"
        Me.RadiationChart.Axes.Bottom.Title.Caption = "Date"
        '
        '
        '
        Me.RadiationChart.Axes.Bottom.Title.Font.Size = 11
        Me.RadiationChart.Axes.Bottom.Title.Font.SizeFloat = 11.0!
        Me.RadiationChart.Axes.Bottom.Title.Lines = New String() {"Date"}
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Axes.Depth.Labels.Bevel.StringColorOne = "FFFFFFFF"
        Me.RadiationChart.Axes.Depth.Labels.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Axes.Depth.Title.Bevel.StringColorOne = "FFFFFFFF"
        Me.RadiationChart.Axes.Depth.Title.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Axes.DepthTop.Labels.Bevel.StringColorOne = "FFFFFFFF"
        Me.RadiationChart.Axes.DepthTop.Labels.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Axes.DepthTop.Title.Bevel.StringColorOne = "FFFFFFFF"
        Me.RadiationChart.Axes.DepthTop.Title.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Axes.Left.AxisPen.Width = 1
        '
        '
        '
        Me.RadiationChart.Axes.Left.Grid.Visible = False
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Axes.Left.Labels.Bevel.StringColorOne = "FFFFFFFF"
        Me.RadiationChart.Axes.Left.Labels.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        Me.RadiationChart.Axes.Left.Labels.Font.Size = 11
        Me.RadiationChart.Axes.Left.Labels.Font.SizeFloat = 11.0!
        Me.RadiationChart.Axes.Left.Labels.Separation = 20
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
        '
        '
        '
        Me.RadiationChart.Axes.Left.Title.Bevel.StringColorOne = "FFFFFFFF"
        Me.RadiationChart.Axes.Left.Title.Bevel.StringColorTwo = "FF808080"
        Me.RadiationChart.Axes.Left.Title.Caption = "Rainfall (mm)"
        '
        '
        '
        Me.RadiationChart.Axes.Left.Title.Font.Size = 11
        Me.RadiationChart.Axes.Left.Title.Font.SizeFloat = 11.0!
        Me.RadiationChart.Axes.Left.Title.Lines = New String() {"Rainfall (mm)"}
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Axes.Right.Grid.Visible = False
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Axes.Right.Labels.Bevel.StringColorOne = "FFFFFFFF"
        Me.RadiationChart.Axes.Right.Labels.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        Me.RadiationChart.Axes.Right.Labels.Font.Size = 11
        Me.RadiationChart.Axes.Right.Labels.Font.SizeFloat = 11.0!
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
        '
        '
        '
        Me.RadiationChart.Axes.Right.Title.Bevel.StringColorOne = "FFFFFFFF"
        Me.RadiationChart.Axes.Right.Title.Bevel.StringColorTwo = "FF808080"
        Me.RadiationChart.Axes.Right.Title.Caption = "Radiation (mJ/m2)"
        '
        '
        '
        Me.RadiationChart.Axes.Right.Title.Font.Size = 11
        Me.RadiationChart.Axes.Right.Title.Font.SizeFloat = 11.0!
        Me.RadiationChart.Axes.Right.Title.Lines = New String() {"Radiation (mJ/m2)"}
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Axes.Top.Labels.Bevel.StringColorOne = "FFFFFFFF"
        Me.RadiationChart.Axes.Top.Labels.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Axes.Top.Title.Bevel.StringColorOne = "FFFFFFFF"
        Me.RadiationChart.Axes.Top.Title.Bevel.StringColorTwo = "FF808080"
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
        Me.RadiationChart.Footer.Bevel.StringColorOne = "FFFFFFFF"
        Me.RadiationChart.Footer.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Header.Bevel.StringColorOne = "FFFFFFFF"
        Me.RadiationChart.Header.Bevel.StringColorTwo = "FF808080"
        Me.RadiationChart.Header.Lines = New String() {""}
        '
        '
        '
        Me.RadiationChart.Legend.Alignment = Steema.TeeChart.LegendAlignments.Bottom
        '
        '
        '
        Me.RadiationChart.Legend.Bevel.StringColorOne = "FFFFFFFF"
        Me.RadiationChart.Legend.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        Me.RadiationChart.Legend.Font.Size = 11
        Me.RadiationChart.Legend.Font.SizeFloat = 11.0!
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
        Me.RadiationChart.Legend.Title.Bevel.StringColorOne = "FFFFFFFF"
        Me.RadiationChart.Legend.Title.Bevel.StringColorTwo = "FF808080"
        Me.RadiationChart.Location = New System.Drawing.Point(0, 0)
        Me.RadiationChart.Name = "RadiationChart"
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Panel.Bevel.Outer = Steema.TeeChart.Drawing.BevelStyles.None
        Me.RadiationChart.Panel.Bevel.StringColorOne = "FFFFFFFF"
        Me.RadiationChart.Panel.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        Me.RadiationChart.Panel.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(0, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        '
        '
        '
        Me.RadiationChart.Panel.ImageBevel.Width = 1
        Me.RadiationChart.Series.Add(Me.RainfallBar2)
        Me.RadiationChart.Series.Add(Me.RadiationLine)
        Me.RadiationChart.Series.Add(Me.MaximumRadiationLine)
        Me.RadiationChart.Size = New System.Drawing.Size(1007, 679)
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.SubFooter.Bevel.StringColorOne = "FFFFFFFF"
        Me.RadiationChart.SubFooter.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.SubHeader.Bevel.StringColorOne = "FFFFFFFF"
        Me.RadiationChart.SubHeader.Bevel.StringColorTwo = "FF808080"
        Me.RadiationChart.TabIndex = 16
        '
        '
        '
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Walls.Back.Bevel.StringColorOne = "FFFFFFFF"
        Me.RadiationChart.Walls.Back.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Walls.Bottom.Bevel.StringColorOne = "FFFFFFFF"
        Me.RadiationChart.Walls.Bottom.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Walls.Left.Bevel.StringColorOne = "FFFFFFFF"
        Me.RadiationChart.Walls.Left.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.RadiationChart.Walls.Right.Bevel.StringColorOne = "FFFFFFFF"
        Me.RadiationChart.Walls.Right.Bevel.StringColorTwo = "FF808080"
        Me.RadiationChart.Walls.Visible = False
        '
        'RainfallBar2
        '
        '
        '
        '
        Me.RainfallBar2.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(0, Byte), Integer), CType(CType(192, Byte), Integer), CType(CType(192, Byte), Integer))
        Me.RainfallBar2.Color = System.Drawing.Color.FromArgb(CType(CType(0, Byte), Integer), CType(CType(192, Byte), Integer), CType(CType(192, Byte), Integer))
        Me.RainfallBar2.ColorEach = False
        '
        '
        '
        '
        '
        '
        Me.RainfallBar2.Marks.Bevel.StringColorOne = "FFFFFFFF"
        Me.RainfallBar2.Marks.Bevel.StringColorTwo = "FF808080"
        '
        '
        '
        '
        '
        '
        Me.RainfallBar2.Marks.Symbol.Bevel.StringColorOne = "FFFFFFFF"
        Me.RainfallBar2.Marks.Symbol.Bevel.StringColorTwo = "FF808080"
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
        Me.RadiationLine.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(68, Byte), Integer), CType(CType(102, Byte), Integer), CType(CType(163, Byte), Integer))
        Me.RadiationLine.Color = System.Drawing.Color.FromArgb(CType(CType(68, Byte), Integer), CType(CType(102, Byte), Integer), CType(CType(163, Byte), Integer))
        Me.RadiationLine.ColorEach = False
        '
        '
        '
        Me.RadiationLine.LinePen.Color = System.Drawing.Color.FromArgb(CType(CType(41, Byte), Integer), CType(CType(61, Byte), Integer), CType(CType(98, Byte), Integer))
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
        Me.MaximumRadiationLine.Brush.Color = System.Drawing.Color.FromArgb(CType(CType(243, Byte), Integer), CType(CType(156, Byte), Integer), CType(CType(53, Byte), Integer))
        Me.MaximumRadiationLine.Color = System.Drawing.Color.FromArgb(CType(CType(243, Byte), Integer), CType(CType(156, Byte), Integer), CType(CType(53, Byte), Integer))
        Me.MaximumRadiationLine.ColorEach = False
        '
        '
        '
        Me.MaximumRadiationLine.LinePen.Color = System.Drawing.Color.FromArgb(CType(CType(146, Byte), Integer), CType(CType(94, Byte), Integer), CType(CType(32, Byte), Integer))
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

            'JF 061211 - Fix bug in max radiation for years that don't begin at day 1 by sending the starting day to QMax
            Dim firstDay As Single
            If (YearlyData.Count > 0) Then
                firstDay = YearlyData.Item(0)("day")
            End If

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
            CalcQmax(firstDay)
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


    Private Sub CalcQmax(ByVal firstDay As Single)
        ' ----------------------------------------------------------------------------------
        ' Add a calculated QMax column to the daily data.
        ' ----------------------------------------------------------------------------------
        If (IsNothing(MetData.Columns("Qmax"))) Then
            MetData.Columns.Add("Qmax")
        End If

        ' Do we have a VP column?
        Dim HaveVPColumn As Boolean = Not IsNothing(MetData.Columns("VP"))

        ' Get latitude for later on.
      Dim Latitude As Single = Convert.ToDouble(Metfile.Constant("latitude").Value, New Globalization.CultureInfo("en-US"))

        ' Loop through all rows and calculate a QMax
        Dim doy As Integer = CType(firstDay, Integer)
        For Row As Integer = 0 To YearlyData.Count - 1
            doy = doy + 1
            If HaveVPColumn AndAlso Not Convert.IsDBNull(YearlyData(Row)("vp")) Then
                YearlyData(Row)("Qmax") = MetUtility.QMax(doy + 1, Latitude, MetUtility.Taz, MetUtility.Alpha, YearlyData(Row)("vp"))
            Else
                YearlyData(Row)("Qmax") = MetUtility.QMax(doy + 1, Latitude, MetUtility.Taz, MetUtility.Alpha, MetUtility.svp(YearlyData(Row)("mint")))
            End If
        Next

    End Sub

End Class
