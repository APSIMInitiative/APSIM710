
using System;
using System.Collections;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Windows.Forms;

using ApsimFile;
using Controllers;
using UIUtility;  //GridUtility.cs


namespace CSUserInterface
	{

	public class SampleUI : BaseView
		{
		private System.Windows.Forms.Panel UnitsPanel;
		private System.Windows.Forms.Splitter splitter1;
		private WaterChartControl waterChartControl1;
        private System.ComponentModel.Container components = null;
		private System.Windows.Forms.DateTimePicker SampleDate;
		private FarPoint.Win.Spread.FpSpread FpSpread;
		private FarPoint.Win.Spread.SheetView Grid;
        private SoilSample MySample;
        private GroupBox groupBox2;
        public Steema.TeeChart.TChart NitrogenChart;
        private Splitter splitter2;
		private bool UserChange = true;
        
		public SampleUI()
			{
			InitializeComponent();
			}

		protected override void Dispose( bool disposing )
			{
			if( disposing )
				{
				if(components != null)
					{
					components.Dispose();
					}
				}
			base.Dispose( disposing );
			}

		#region Component Designer generated code
		/// <summary> 
		/// Required method for Designer support - do not modify 
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
        FarPoint.Win.Spread.TipAppearance tipAppearance2 = new FarPoint.Win.Spread.TipAppearance();
        this.FpSpread = new FarPoint.Win.Spread.FpSpread();
        this.Grid = new FarPoint.Win.Spread.SheetView();
        this.UnitsPanel = new System.Windows.Forms.Panel();
        this.SampleDate = new System.Windows.Forms.DateTimePicker();
        this.splitter1 = new System.Windows.Forms.Splitter();
        this.waterChartControl1 = new CSUserInterface.WaterChartControl();
        this.groupBox2 = new System.Windows.Forms.GroupBox();
        this.NitrogenChart = new Steema.TeeChart.TChart();
        this.splitter2 = new System.Windows.Forms.Splitter();
        ((System.ComponentModel.ISupportInitialize)(this.FpSpread)).BeginInit();
        ((System.ComponentModel.ISupportInitialize)(this.Grid)).BeginInit();
        this.UnitsPanel.SuspendLayout();
        this.SuspendLayout();
        // 
        // FpSpread
        // 
        this.FpSpread.AccessibleDescription = "FpSpread, Water / Nitrogen, Row 0, Column 0, ";
        this.FpSpread.AllowDragDrop = true;
        this.FpSpread.Dock = System.Windows.Forms.DockStyle.Top;
        this.FpSpread.EditModeReplace = true;
        this.FpSpread.HorizontalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded;
        this.FpSpread.Location = new System.Drawing.Point(0, 108);
        this.FpSpread.Name = "FpSpread";
        this.FpSpread.Sheets.AddRange(new FarPoint.Win.Spread.SheetView[] {
            this.Grid});
        this.FpSpread.Size = new System.Drawing.Size(696, 238);
        this.FpSpread.TabIndex = 2;
        tipAppearance2.BackColor = System.Drawing.SystemColors.Info;
        tipAppearance2.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
        tipAppearance2.ForeColor = System.Drawing.SystemColors.InfoText;
        this.FpSpread.TextTipAppearance = tipAppearance2;
        this.FpSpread.VerticalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded;
        // 
        // Grid
        // 
        this.Grid.Reset();
        // Formulas and custom names must be loaded with R1C1 reference style
        this.Grid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1;
        this.Grid.ColumnCount = 13;
        this.Grid.ColumnHeader.RowCount = 2;
        this.Grid.RowCount = 100;
        this.Grid.AutoUpdateNotes = true;
        this.Grid.ColumnHeader.AutoText = FarPoint.Win.Spread.HeaderAutoText.Blank;
        this.Grid.ColumnHeader.Cells.Get(0, 0).Value = "Depth";
        this.Grid.ColumnHeader.Cells.Get(0, 1).Value = "Wet";
        this.Grid.ColumnHeader.Cells.Get(0, 2).Value = "Dry";
        this.Grid.ColumnHeader.Cells.Get(0, 3).Value = "SW";
        this.Grid.ColumnHeader.Cells.Get(0, 4).Value = "SW";
        this.Grid.ColumnHeader.Cells.Get(0, 5).Value = "NO3";
        this.Grid.ColumnHeader.Cells.Get(0, 6).Value = "NH4";
        this.Grid.ColumnHeader.Cells.Get(0, 7).Value = "NO3";
        this.Grid.ColumnHeader.Cells.Get(0, 8).Value = "NH4";
        this.Grid.ColumnHeader.Cells.Get(0, 9).Value = "OC";
        this.Grid.ColumnHeader.Cells.Get(0, 10).Value = "EC";
        this.Grid.ColumnHeader.Cells.Get(0, 11).Value = "pH";
        this.Grid.ColumnHeader.Cells.Get(0, 12).Value = "ESP";
        this.Grid.ColumnHeader.Cells.Get(1, 0).Value = "(cm)";
        this.Grid.ColumnHeader.Cells.Get(1, 1).Value = "(grav%)";
        this.Grid.ColumnHeader.Cells.Get(1, 2).Value = "(grav%)";
        this.Grid.ColumnHeader.Cells.Get(1, 3).Value = "(grav%)";
        this.Grid.ColumnHeader.Cells.Get(1, 4).Value = "(%vol)";
        this.Grid.ColumnHeader.Cells.Get(1, 5).Value = "(ppm)";
        this.Grid.ColumnHeader.Cells.Get(1, 6).Value = "(ppm)";
        this.Grid.ColumnHeader.Cells.Get(1, 7).Value = "(kg/ha)";
        this.Grid.ColumnHeader.Cells.Get(1, 8).Value = "(kg/ha)";
        this.Grid.ColumnHeader.Cells.Get(1, 9).Value = "(%C)";
        this.Grid.ColumnHeader.Cells.Get(1, 10).Value = "(dS/m)";
        this.Grid.ColumnHeader.Cells.Get(1, 11).Value = "(CaCl2)";
        this.Grid.ColumnHeader.Cells.Get(1, 12).Value = "(%)";
        this.Grid.Columns.Get(1).BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(192)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
        this.Grid.Columns.Get(1).Label = "(grav%)";
        this.Grid.Columns.Get(2).BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(192)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
        this.Grid.Columns.Get(2).Label = "(grav%)";
        this.Grid.Columns.Get(3).BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(192)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
        this.Grid.Columns.Get(3).Label = "(grav%)";
        this.Grid.Columns.Get(4).BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(192)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
        this.Grid.Columns.Get(4).Label = "(%vol)";
        this.Grid.Columns.Get(5).BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(224)))), ((int)(((byte)(192)))));
        this.Grid.Columns.Get(5).Label = "(ppm)";
        this.Grid.Columns.Get(6).BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(224)))), ((int)(((byte)(192)))));
        this.Grid.Columns.Get(6).Label = "(ppm)";
        this.Grid.Columns.Get(7).BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(224)))), ((int)(((byte)(192)))));
        this.Grid.Columns.Get(7).Label = "(kg/ha)";
        this.Grid.Columns.Get(8).BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(224)))), ((int)(((byte)(192)))));
        this.Grid.Columns.Get(8).Label = "(kg/ha)";
        this.Grid.RowHeader.Columns.Default.Resizable = false;
        this.Grid.RowHeader.Visible = false;
        this.Grid.SheetName = "Water / Nitrogen";
        this.Grid.CellChanged += new FarPoint.Win.Spread.SheetViewEventHandler(this.Grid_CellChanged);
        this.Grid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1;
        // 
        // UnitsPanel
        // 
        this.UnitsPanel.Controls.Add(this.SampleDate);
        this.UnitsPanel.Controls.Add(this.groupBox2);
        this.UnitsPanel.Dock = System.Windows.Forms.DockStyle.Top;
        this.UnitsPanel.Location = new System.Drawing.Point(0, 40);
        this.UnitsPanel.Name = "UnitsPanel";
        this.UnitsPanel.Size = new System.Drawing.Size(696, 68);
        this.UnitsPanel.TabIndex = 3;
        // 
        // SampleDate
        // 
        this.SampleDate.Format = System.Windows.Forms.DateTimePickerFormat.Short;
        this.SampleDate.Location = new System.Drawing.Point(7, 23);
        this.SampleDate.Name = "SampleDate";
        this.SampleDate.Size = new System.Drawing.Size(96, 20);
        this.SampleDate.TabIndex = 1;
        this.SampleDate.ValueChanged += new System.EventHandler(this.SampleDate_ValueChanged);
        // 
        // splitter1
        // 
        this.splitter1.Dock = System.Windows.Forms.DockStyle.Top;
        this.splitter1.Location = new System.Drawing.Point(0, 346);
        this.splitter1.Name = "splitter1";
        this.splitter1.Size = new System.Drawing.Size(696, 3);
        this.splitter1.TabIndex = 4;
        this.splitter1.TabStop = false;
        // 
        // waterChartControl1
        // 
        this.waterChartControl1.Dock = System.Windows.Forms.DockStyle.Left;
        this.waterChartControl1.LinkedSoil = null;
        this.waterChartControl1.Location = new System.Drawing.Point(0, 349);
        this.waterChartControl1.Name = "waterChartControl1";
        this.waterChartControl1.Size = new System.Drawing.Size(348, 428);
        this.waterChartControl1.TabIndex = 5;
        // 
        // groupBox2
        // 
        this.groupBox2.BackColor = System.Drawing.SystemColors.ControlLight;
        this.groupBox2.Location = new System.Drawing.Point(3, 3);
        this.groupBox2.Name = "groupBox2";
        this.groupBox2.Size = new System.Drawing.Size(106, 60);
        this.groupBox2.TabIndex = 9;
        this.groupBox2.TabStop = false;
        this.groupBox2.Text = "Sample date";
        // 
        // NitrogenChart
        // 
        // 
        // 
        // 
        this.NitrogenChart.Aspect.ElevationFloat = 345;
        this.NitrogenChart.Aspect.RotationFloat = 345;
        this.NitrogenChart.Aspect.View3D = false;
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Axes.Bottom.Automatic = true;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Bottom.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
        this.NitrogenChart.Axes.Bottom.Grid.Visible = false;
        this.NitrogenChart.Axes.Bottom.Grid.ZPosition = 0;
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Axes.Bottom.Labels.Font.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Bottom.Labels.Shadow.Visible = false;
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Axes.Bottom.Title.Font.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Bottom.Title.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Depth.Automatic = true;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Depth.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
        this.NitrogenChart.Axes.Depth.Grid.ZPosition = 0;
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Axes.Depth.Labels.Font.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Depth.Labels.Shadow.Visible = false;
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Axes.Depth.Title.Font.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Depth.Title.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.DepthTop.Automatic = true;
        // 
        // 
        // 
        this.NitrogenChart.Axes.DepthTop.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
        this.NitrogenChart.Axes.DepthTop.Grid.ZPosition = 0;
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Axes.DepthTop.Labels.Font.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.DepthTop.Labels.Shadow.Visible = false;
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Axes.DepthTop.Title.Font.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.DepthTop.Title.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Left.AutomaticMinimum = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Left.AxisPen.Width = 1;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Left.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
        this.NitrogenChart.Axes.Left.Grid.Visible = false;
        this.NitrogenChart.Axes.Left.Grid.ZPosition = 0;
        this.NitrogenChart.Axes.Left.Increment = 20;
        this.NitrogenChart.Axes.Left.Inverted = true;
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Axes.Left.Labels.Font.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Left.Labels.Shadow.Visible = false;
        this.NitrogenChart.Axes.Left.MaximumOffset = 2;
        this.NitrogenChart.Axes.Left.Minimum = 0;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Left.MinorTicks.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Left.Ticks.Length = 0;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Left.TicksInner.Length = 5;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Left.Title.Caption = "Depth (cm)";
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Axes.Left.Title.Font.Shadow.Visible = false;
        this.NitrogenChart.Axes.Left.Title.Lines = new string[] {
        "Depth (cm)"};
        // 
        // 
        // 
        this.NitrogenChart.Axes.Left.Title.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Right.Automatic = true;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Right.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
        this.NitrogenChart.Axes.Right.Grid.ZPosition = 0;
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Axes.Right.Labels.Font.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Right.Labels.Shadow.Visible = false;
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Axes.Right.Title.Font.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Right.Title.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Top.Automatic = true;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Top.AxisPen.Width = 1;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Top.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
        this.NitrogenChart.Axes.Top.Grid.Visible = false;
        this.NitrogenChart.Axes.Top.Grid.ZPosition = 0;
        this.NitrogenChart.Axes.Top.Increment = 2;
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Axes.Top.Labels.Font.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Top.Labels.Shadow.Visible = false;
        this.NitrogenChart.Axes.Top.MaximumOffset = 2;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Top.MinorTicks.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Top.Ticks.Length = 0;
        this.NitrogenChart.Axes.Top.Ticks.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Top.TicksInner.Length = 5;
        // 
        // 
        // 
        this.NitrogenChart.Axes.Top.Title.Caption = "Nitrogen (kg/ha)";
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Axes.Top.Title.Font.Shadow.Visible = false;
        this.NitrogenChart.Axes.Top.Title.Lines = new string[] {
        "Nitrogen (kg/ha)"};
        // 
        // 
        // 
        this.NitrogenChart.Axes.Top.Title.Shadow.Visible = false;
        this.NitrogenChart.Cursor = System.Windows.Forms.Cursors.Default;
        this.NitrogenChart.Dock = System.Windows.Forms.DockStyle.Fill;
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Footer.Font.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Footer.Shadow.Visible = false;
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Header.Font.Shadow.Visible = false;
        this.NitrogenChart.Header.Lines = new string[] {
        ""};
        // 
        // 
        // 
        this.NitrogenChart.Header.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Legend.CheckBoxes = true;
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Legend.Font.Shadow.Visible = false;
        this.NitrogenChart.Legend.LegendStyle = Steema.TeeChart.LegendStyles.Series;
        // 
        // 
        // 
        this.NitrogenChart.Legend.Pen.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Legend.Shadow.Visible = false;
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Legend.Title.Font.Bold = true;
        // 
        // 
        // 
        this.NitrogenChart.Legend.Title.Font.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Legend.Title.Pen.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Legend.Title.Shadow.Visible = false;
        this.NitrogenChart.Location = new System.Drawing.Point(348, 349);
        this.NitrogenChart.Name = "NitrogenChart";
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Panel.Bevel.Outer = Steema.TeeChart.Drawing.BevelStyles.None;
        // 
        // 
        // 
        this.NitrogenChart.Panel.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
        // 
        // 
        // 
        this.NitrogenChart.Panel.ImageBevel.Width = 1;
        // 
        // 
        // 
        this.NitrogenChart.Panel.Shadow.Visible = false;
        this.NitrogenChart.Size = new System.Drawing.Size(348, 428);
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.SubFooter.Font.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.SubFooter.Shadow.Visible = false;
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.SubHeader.Font.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.SubHeader.Shadow.Visible = false;
        this.NitrogenChart.TabIndex = 39;
        // 
        // 
        // 
        // 
        // 
        // 
        this.NitrogenChart.Walls.Back.AutoHide = false;
        // 
        // 
        // 
        this.NitrogenChart.Walls.Back.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Walls.Bottom.AutoHide = false;
        // 
        // 
        // 
        this.NitrogenChart.Walls.Bottom.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Walls.Left.AutoHide = false;
        // 
        // 
        // 
        this.NitrogenChart.Walls.Left.Shadow.Visible = false;
        // 
        // 
        // 
        this.NitrogenChart.Walls.Right.AutoHide = false;
        // 
        // 
        // 
        this.NitrogenChart.Walls.Right.Shadow.Visible = false;
        this.NitrogenChart.Walls.Visible = false;
        // 
        // splitter2
        // 
        this.splitter2.Location = new System.Drawing.Point(348, 349);
        this.splitter2.Name = "splitter2";
        this.splitter2.Size = new System.Drawing.Size(3, 428);
        this.splitter2.TabIndex = 40;
        this.splitter2.TabStop = false;
        // 
        // SampleUI
        // 
        this.Controls.Add(this.splitter2);
        this.Controls.Add(this.NitrogenChart);
        this.Controls.Add(this.waterChartControl1);
        this.Controls.Add(this.splitter1);
        this.Controls.Add(this.FpSpread);
        this.Controls.Add(this.UnitsPanel);
        this.Name = "SampleUI";
        this.Size = new System.Drawing.Size(696, 777);
        this.Controls.SetChildIndex(this.UnitsPanel, 0);
        this.Controls.SetChildIndex(this.FpSpread, 0);
        this.Controls.SetChildIndex(this.splitter1, 0);
        this.Controls.SetChildIndex(this.waterChartControl1, 0);
        this.Controls.SetChildIndex(this.NitrogenChart, 0);
        this.Controls.SetChildIndex(this.splitter2, 0);
        ((System.ComponentModel.ISupportInitialize)(this.FpSpread)).EndInit();
        ((System.ComponentModel.ISupportInitialize)(this.Grid)).EndInit();
        this.UnitsPanel.ResumeLayout(false);
        this.ResumeLayout(false);

		}
		#endregion
        protected override void OnLoad()
            {
            FarPoint.Win.Spread.InputMap InputMap = FpSpread.GetInputMap(FarPoint.Win.Spread.InputMapMode.WhenAncestorOfFocused);
            InputMap.Put(new FarPoint.Win.Spread.Keystroke(Keys.Delete, Keys.None),
                        FarPoint.Win.Spread.SpreadActions.ClipboardCut);
            InputMap.Put(new FarPoint.Win.Spread.Keystroke(Keys.Enter, Keys.None),
                        FarPoint.Win.Spread.SpreadActions.MoveToNextRow);
            }
		override public void OnRefresh()
			{
			try
				{
				MySample = new SoilSample(Data, new Soil(Data.ParentNode));
				PopulateGrid();
				}
			catch (Exception err)
				{
				MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
				}
			}

		private void PopulateGrid()
			{
			UserChange = false;
			Grid.RowCount = 1;
			Grid.RowCount = 20;
			GridUtility.SetColumnAsStrings(Grid, 0, SoilComponentUtility.ToDepthStrings(MySample.Thickness));

			SampleDate.Value = MySample.SampleDate;
			if (MySample.WaterFormat == "VolumetricPercent")
				{
				GridUtility.SetColumnAsDoubles(Grid, 1, MySample.SW);
				Grid.Columns[2].Visible = false;
				Grid.ColumnHeader.Cells[0, 1].Text = "Water";
				Grid.ColumnHeader.Cells[1, 1].Text = "(%vol)";
				}
			else if (MySample.WaterFormat == "GravimetricPercent")
				{
				GridUtility.SetColumnAsDoubles(Grid, 1, MySample.SWGrav);
				Grid.Columns[2].Visible = false;
				Grid.ColumnHeader.Cells[0, 1].Text = "Water";
				Grid.ColumnHeader.Cells[1, 1].Text = "(%vol)";
				}
			else
				{
				GridUtility.SetColumnAsDoubles(Grid, 1, MySample.Wet);
				Grid.Columns[2].Visible = true;
				Grid.ColumnHeader.Cells[0, 1].Text = "Wet";
				Grid.ColumnHeader.Cells[1, 1].Text = "(%grav)";
				Grid.ColumnHeader.Cells[0, 2].Text = "Dry";
				Grid.ColumnHeader.Cells[1, 2].Text = "(%grav)";
				}
			GridUtility.SetColumnAsDoubles(Grid, 3, MySample.NO3);
			GridUtility.SetColumnAsDoubles(Grid, 4, MySample.NH4);
			GridUtility.SetColumnAsDoubles(Grid, 5, MySample.OC);
			GridUtility.SetColumnAsDoubles(Grid, 6, MySample.EC);
			GridUtility.SetColumnAsDoubles(Grid, 7, MySample.PH);
			GridUtility.SetColumnAsDoubles(Grid, 8, MySample.ESP);
			UserChange = true;
			}								   

		private void SaveGrid()
			{
/*			int NumLayers = GridUtility.FindFirstBlankCell(Grid, 0);
			MySample.Thickness = Soils.SoilComponentUtility.ToThickness(GridUtility.GetColumnAsStrings(Grid, 0, NumLayers));
			if (WaterUnits.SelectedIndex == 0)
                MySample.SW = GridUtility.GetColumnAsDoubles(Grid, 1, NumLayers);
			else if (WaterUnits.SelectedIndex == 1)
                MySample.SWGrav = GridUtility.GetColumnAsDoubles(Grid, 1, NumLayers);
			else if (WaterUnits.SelectedIndex == 2)
				{
                MySample.Wet = GridUtility.GetColumnAsDoubles(Grid, 1, NumLayers);
                MySample.Dry = GridUtility.GetColumnAsDoubles(Grid, 2, NumLayers);
				}

            MySample.NO3 = GridUtility.GetColumnAsDoubles(Grid, 3, NumLayers);
            MySample.NH4 = GridUtility.GetColumnAsDoubles(Grid, 4, NumLayers);
            MySample.OC = GridUtility.GetColumnAsDoubles(Grid, 5, NumLayers);
            MySample.EC = GridUtility.GetColumnAsDoubles(Grid, 6, NumLayers);
            MySample.PH = GridUtility.GetColumnAsDoubles(Grid, 7, NumLayers);
            MySample.ESP = GridUtility.GetColumnAsDoubles(Grid, 8, NumLayers);
*/			}

		private void SampleDate_ValueChanged(object sender, System.EventArgs e)
			{
			MySample.SampleDate = SampleDate.Value;
			}

		private void WaterUnits_SelectedIndexChanged(object sender, System.EventArgs e)
			{
			if (UserChange)
				{
				SaveGrid();
				PopulateGrid();
				}
			}

		private void Grid_CellChanged(object sender, FarPoint.Win.Spread.SheetViewEventArgs e)
			{
			if (UserChange)
                SaveGrid();
			}


		}
	}
