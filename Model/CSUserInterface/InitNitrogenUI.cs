
using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using System.Xml;

using ApsimFile;
using Controllers;
using CSGeneral;
using UIUtility;  //GridUtility.cs


namespace CSUserInterface
	{
	public class InitNitrogenUI : BaseView
		{
		private System.ComponentModel.IContainer components = null;
        private System.Windows.Forms.Panel panel1;
		private bool UserChange = true;
        private InitNitrogen InitialNitrogen = null;
        public Steema.TeeChart.TChart NitrogenChart;
        private Steema.TeeChart.Styles.Line No3KgHaLine;
        private Steema.TeeChart.Styles.Line Nh4PpmLine;
        private Steema.TeeChart.Styles.Line Nh4KgHaLine;
        private Steema.TeeChart.Styles.Line No3PpmLine;
        private Panel LeftHandPanel;
        private FarPoint.Win.Spread.FpSpread Grid;
        private FarPoint.Win.Spread.SheetView NitrogenGrid;
        private Panel panel3;
        private Label label4;
        private TextBox TotalNH4;
        private Label label3;
        private TextBox TotalNO3;
        private Label label2;
        private Label label1;
		private Soil SoilData;

		#region Constructor / Destructor
		public InitNitrogenUI()
			{
			InitializeComponent();
			}

		protected override void Dispose( bool disposing )
			{
			if( disposing )
				{
				if (components != null) 
					{
					components.Dispose();
					}
				}
			base.Dispose( disposing );
			}

		#region Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
        System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(InitNitrogenUI));
        FarPoint.Win.Spread.TipAppearance tipAppearance1 = new FarPoint.Win.Spread.TipAppearance();
        FarPoint.Win.Spread.CellType.NumberCellType numberCellType1 = new FarPoint.Win.Spread.CellType.NumberCellType();
        FarPoint.Win.Spread.CellType.NumberCellType numberCellType2 = new FarPoint.Win.Spread.CellType.NumberCellType();
        FarPoint.Win.Spread.CellType.NumberCellType numberCellType3 = new FarPoint.Win.Spread.CellType.NumberCellType();
        FarPoint.Win.Spread.CellType.NumberCellType numberCellType4 = new FarPoint.Win.Spread.CellType.NumberCellType();
        this.panel1 = new System.Windows.Forms.Panel();
        this.NitrogenChart = new Steema.TeeChart.TChart();
        this.No3KgHaLine = new Steema.TeeChart.Styles.Line();
        this.Nh4KgHaLine = new Steema.TeeChart.Styles.Line();
        this.No3PpmLine = new Steema.TeeChart.Styles.Line();
        this.Nh4PpmLine = new Steema.TeeChart.Styles.Line();
        this.LeftHandPanel = new System.Windows.Forms.Panel();
        this.Grid = new FarPoint.Win.Spread.FpSpread();
        this.NitrogenGrid = new FarPoint.Win.Spread.SheetView();
        this.panel3 = new System.Windows.Forms.Panel();
        this.label4 = new System.Windows.Forms.Label();
        this.TotalNH4 = new System.Windows.Forms.TextBox();
        this.label3 = new System.Windows.Forms.Label();
        this.TotalNO3 = new System.Windows.Forms.TextBox();
        this.label2 = new System.Windows.Forms.Label();
        this.label1 = new System.Windows.Forms.Label();
        this.panel1.SuspendLayout();
        this.LeftHandPanel.SuspendLayout();
        ((System.ComponentModel.ISupportInitialize)(this.Grid)).BeginInit();
        ((System.ComponentModel.ISupportInitialize)(this.NitrogenGrid)).BeginInit();
        this.panel3.SuspendLayout();
        this.SuspendLayout();
        // 
        // panel1
        // 
        this.panel1.Controls.Add(this.NitrogenChart);
        this.panel1.Controls.Add(this.LeftHandPanel);
        this.panel1.Dock = System.Windows.Forms.DockStyle.Fill;
        this.panel1.Location = new System.Drawing.Point(0, 40);
        this.panel1.Name = "panel1";
        this.panel1.Size = new System.Drawing.Size(908, 725);
        this.panel1.TabIndex = 36;
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
        this.NitrogenChart.Location = new System.Drawing.Point(322, 0);
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
        this.NitrogenChart.Series.Add(this.No3KgHaLine);
        this.NitrogenChart.Series.Add(this.Nh4KgHaLine);
        this.NitrogenChart.Series.Add(this.No3PpmLine);
        this.NitrogenChart.Series.Add(this.Nh4PpmLine);
        this.NitrogenChart.Size = new System.Drawing.Size(586, 725);
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
        this.NitrogenChart.TabIndex = 38;
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
        this.NitrogenChart.ClickLegend += new System.Windows.Forms.MouseEventHandler(this.NitrogenChart_ClickLegend);
        // 
        // No3KgHaLine
        // 
        // 
        // 
        // 
        this.No3KgHaLine.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(128)))), ((int)(((byte)(0)))));
        this.No3KgHaLine.HorizAxis = Steema.TeeChart.Styles.HorizontalAxis.Top;
        // 
        // 
        // 
        this.No3KgHaLine.LinePen.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(77)))), ((int)(((byte)(0)))));
        this.No3KgHaLine.LinePen.Width = 2;
        // 
        // 
        // 
        // 
        // 
        // 
        this.No3KgHaLine.Marks.Callout.ArrowHead = Steema.TeeChart.Styles.ArrowHeadStyles.None;
        this.No3KgHaLine.Marks.Callout.ArrowHeadSize = 8;
        // 
        // 
        // 
        this.No3KgHaLine.Marks.Callout.Brush.Color = System.Drawing.Color.Black;
        this.No3KgHaLine.Marks.Callout.Distance = 0;
        this.No3KgHaLine.Marks.Callout.Draw3D = false;
        this.No3KgHaLine.Marks.Callout.Length = 10;
        this.No3KgHaLine.Marks.Callout.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
        // 
        // 
        // 
        // 
        // 
        // 
        this.No3KgHaLine.Marks.Font.Shadow.Visible = false;
        // 
        // 
        // 
        // 
        // 
        // 
        this.No3KgHaLine.Pointer.Brush.Color = System.Drawing.Color.Red;
        this.No3KgHaLine.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
        this.No3KgHaLine.Title = "no3 (kg/ha)";
        // 
        // 
        // 
        this.No3KgHaLine.XValues.DataMember = "X";
        this.No3KgHaLine.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending;
        // 
        // 
        // 
        this.No3KgHaLine.YValues.DataMember = "Y";
        // 
        // Nh4KgHaLine
        // 
        // 
        // 
        // 
        this.Nh4KgHaLine.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(128)))), ((int)(((byte)(0)))));
        this.Nh4KgHaLine.HorizAxis = Steema.TeeChart.Styles.HorizontalAxis.Top;
        // 
        // 
        // 
        this.Nh4KgHaLine.LinePen.Color = System.Drawing.Color.FromArgb(((int)(((byte)(153)))), ((int)(((byte)(77)))), ((int)(((byte)(0)))));
        this.Nh4KgHaLine.LinePen.Width = 2;
        // 
        // 
        // 
        // 
        // 
        // 
        this.Nh4KgHaLine.Marks.Callout.ArrowHead = Steema.TeeChart.Styles.ArrowHeadStyles.None;
        this.Nh4KgHaLine.Marks.Callout.ArrowHeadSize = 8;
        // 
        // 
        // 
        this.Nh4KgHaLine.Marks.Callout.Brush.Color = System.Drawing.Color.Black;
        this.Nh4KgHaLine.Marks.Callout.Distance = 0;
        this.Nh4KgHaLine.Marks.Callout.Draw3D = false;
        this.Nh4KgHaLine.Marks.Callout.Length = 10;
        this.Nh4KgHaLine.Marks.Callout.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
        // 
        // 
        // 
        // 
        // 
        // 
        this.Nh4KgHaLine.Marks.Font.Shadow.Visible = false;
        // 
        // 
        // 
        // 
        // 
        // 
        this.Nh4KgHaLine.Pointer.Brush.Color = System.Drawing.Color.Red;
        this.Nh4KgHaLine.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
        this.Nh4KgHaLine.Title = "nh4 (kg/ha)";
        // 
        // 
        // 
        this.Nh4KgHaLine.XValues.DataMember = "X";
        this.Nh4KgHaLine.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending;
        // 
        // 
        // 
        this.Nh4KgHaLine.YValues.DataMember = "Y";
        // 
        // No3PpmLine
        // 
        // 
        // 
        // 
        this.No3PpmLine.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(128)))), ((int)(((byte)(0)))));
        this.No3PpmLine.HorizAxis = Steema.TeeChart.Styles.HorizontalAxis.Top;
        // 
        // 
        // 
        this.No3PpmLine.LinePen.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(77)))), ((int)(((byte)(0)))));
        this.No3PpmLine.LinePen.Width = 2;
        // 
        // 
        // 
        // 
        // 
        // 
        this.No3PpmLine.Marks.Callout.ArrowHead = Steema.TeeChart.Styles.ArrowHeadStyles.None;
        this.No3PpmLine.Marks.Callout.ArrowHeadSize = 8;
        // 
        // 
        // 
        this.No3PpmLine.Marks.Callout.Brush.Color = System.Drawing.Color.Black;
        this.No3PpmLine.Marks.Callout.Distance = 0;
        this.No3PpmLine.Marks.Callout.Draw3D = false;
        this.No3PpmLine.Marks.Callout.Length = 10;
        this.No3PpmLine.Marks.Callout.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
        // 
        // 
        // 
        // 
        // 
        // 
        this.No3PpmLine.Marks.Font.Shadow.Visible = false;
        // 
        // 
        // 
        // 
        // 
        // 
        this.No3PpmLine.Pointer.Brush.Color = System.Drawing.Color.Red;
        this.No3PpmLine.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
        this.No3PpmLine.Title = "no3 (ppm)";
        this.No3PpmLine.Visible = false;
        // 
        // 
        // 
        this.No3PpmLine.XValues.DataMember = "X";
        this.No3PpmLine.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending;
        // 
        // 
        // 
        this.No3PpmLine.YValues.DataMember = "Y";
        // 
        // Nh4PpmLine
        // 
        // 
        // 
        // 
        this.Nh4PpmLine.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(128)))), ((int)(((byte)(0)))));
        this.Nh4PpmLine.HorizAxis = Steema.TeeChart.Styles.HorizontalAxis.Top;
        // 
        // 
        // 
        this.Nh4PpmLine.LinePen.Color = System.Drawing.Color.FromArgb(((int)(((byte)(153)))), ((int)(((byte)(77)))), ((int)(((byte)(0)))));
        this.Nh4PpmLine.LinePen.Width = 2;
        // 
        // 
        // 
        // 
        // 
        // 
        this.Nh4PpmLine.Marks.Callout.ArrowHead = Steema.TeeChart.Styles.ArrowHeadStyles.None;
        this.Nh4PpmLine.Marks.Callout.ArrowHeadSize = 8;
        // 
        // 
        // 
        this.Nh4PpmLine.Marks.Callout.Brush.Color = System.Drawing.Color.Black;
        this.Nh4PpmLine.Marks.Callout.Distance = 0;
        this.Nh4PpmLine.Marks.Callout.Draw3D = false;
        this.Nh4PpmLine.Marks.Callout.Length = 10;
        this.Nh4PpmLine.Marks.Callout.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
        // 
        // 
        // 
        // 
        // 
        // 
        this.Nh4PpmLine.Marks.Font.Shadow.Visible = false;
        // 
        // 
        // 
        // 
        // 
        // 
        this.Nh4PpmLine.Pointer.Brush.Color = System.Drawing.Color.Red;
        this.Nh4PpmLine.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
        this.Nh4PpmLine.Title = "nh4 (ppm)";
        this.Nh4PpmLine.Visible = false;
        // 
        // 
        // 
        this.Nh4PpmLine.XValues.DataMember = "X";
        this.Nh4PpmLine.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending;
        // 
        // 
        // 
        this.Nh4PpmLine.YValues.DataMember = "Y";
        // 
        // LeftHandPanel
        // 
        this.LeftHandPanel.Controls.Add(this.Grid);
        this.LeftHandPanel.Controls.Add(this.panel3);
        this.LeftHandPanel.Dock = System.Windows.Forms.DockStyle.Left;
        this.LeftHandPanel.Location = new System.Drawing.Point(0, 0);
        this.LeftHandPanel.Name = "LeftHandPanel";
        this.LeftHandPanel.Size = new System.Drawing.Size(322, 725);
        this.LeftHandPanel.TabIndex = 39;
        // 
        // Grid
        // 
        this.Grid.AccessibleDescription = "Grid, Sheet1, Row 0, Column 0, ";
        this.Grid.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
        this.Grid.Dock = System.Windows.Forms.DockStyle.Fill;
        this.Grid.EditModeReplace = true;
        this.Grid.HorizontalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded;
        this.Grid.Location = new System.Drawing.Point(0, 68);
        this.Grid.Name = "Grid";
        this.Grid.Sheets.AddRange(new FarPoint.Win.Spread.SheetView[] {
            this.NitrogenGrid});
        this.Grid.Size = new System.Drawing.Size(322, 657);
        this.Grid.TabIndex = 37;
        tipAppearance1.BackColor = System.Drawing.SystemColors.Info;
        tipAppearance1.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
        tipAppearance1.ForeColor = System.Drawing.SystemColors.InfoText;
        this.Grid.TextTipAppearance = tipAppearance1;
        this.Grid.VerticalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded;
        // 
        // NitrogenGrid
        // 
        this.NitrogenGrid.Reset();
        // Formulas and custom names must be loaded with R1C1 reference style
        this.NitrogenGrid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1;
        this.NitrogenGrid.ColumnCount = 5;
        this.NitrogenGrid.ColumnHeader.RowCount = 2;
        this.NitrogenGrid.AutoUpdateNotes = true;
        this.NitrogenGrid.ColumnHeader.Cells.Get(0, 0).Value = "Depth";
        this.NitrogenGrid.ColumnHeader.Cells.Get(0, 1).Value = "NO3";
        this.NitrogenGrid.ColumnHeader.Cells.Get(0, 2).Value = "NH4";
        this.NitrogenGrid.ColumnHeader.Cells.Get(0, 3).Value = "NO3";
        this.NitrogenGrid.ColumnHeader.Cells.Get(0, 4).Value = "NH4";
        this.NitrogenGrid.ColumnHeader.Cells.Get(1, 0).Value = "(cm)";
        this.NitrogenGrid.ColumnHeader.Cells.Get(1, 1).Value = "(kg/ha)";
        this.NitrogenGrid.ColumnHeader.Cells.Get(1, 2).Value = "(kg/ha)";
        this.NitrogenGrid.ColumnHeader.Cells.Get(1, 3).Value = "(ppm)";
        this.NitrogenGrid.ColumnHeader.Cells.Get(1, 4).Value = "(ppm)";
        this.NitrogenGrid.Columns.Get(0).BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(224)))), ((int)(((byte)(224)))), ((int)(((byte)(224)))));
        this.NitrogenGrid.Columns.Get(0).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
        this.NitrogenGrid.Columns.Get(0).Label = "(cm)";
        this.NitrogenGrid.Columns.Get(0).Locked = true;
        numberCellType1.DecimalPlaces = 3;
        this.NitrogenGrid.Columns.Get(1).CellType = numberCellType1;
        this.NitrogenGrid.Columns.Get(1).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
        this.NitrogenGrid.Columns.Get(1).Label = "(kg/ha)";
        numberCellType2.DecimalPlaces = 3;
        this.NitrogenGrid.Columns.Get(2).CellType = numberCellType2;
        this.NitrogenGrid.Columns.Get(2).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
        this.NitrogenGrid.Columns.Get(2).Label = "(kg/ha)";
        this.NitrogenGrid.Columns.Get(3).BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(192)))));
        numberCellType3.DecimalPlaces = 3;
        this.NitrogenGrid.Columns.Get(3).CellType = numberCellType3;
        this.NitrogenGrid.Columns.Get(3).Label = "(ppm)";
        this.NitrogenGrid.Columns.Get(4).BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(192)))));
        numberCellType4.DecimalPlaces = 3;
        this.NitrogenGrid.Columns.Get(4).CellType = numberCellType4;
        this.NitrogenGrid.Columns.Get(4).Label = "(ppm)";
        this.NitrogenGrid.RowHeader.Columns.Default.Resizable = false;
        this.NitrogenGrid.RowHeader.Visible = false;
        this.NitrogenGrid.SheetName = "Sheet1";
        this.NitrogenGrid.CellChanged += new FarPoint.Win.Spread.SheetViewEventHandler(this.NitrogenGrid_CellChanged);
        this.NitrogenGrid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1;
        // 
        // panel3
        // 
        this.panel3.Controls.Add(this.label4);
        this.panel3.Controls.Add(this.TotalNH4);
        this.panel3.Controls.Add(this.label3);
        this.panel3.Controls.Add(this.TotalNO3);
        this.panel3.Controls.Add(this.label2);
        this.panel3.Controls.Add(this.label1);
        this.panel3.Dock = System.Windows.Forms.DockStyle.Top;
        this.panel3.Location = new System.Drawing.Point(0, 0);
        this.panel3.Name = "panel3";
        this.panel3.Size = new System.Drawing.Size(322, 68);
        this.panel3.TabIndex = 38;
        // 
        // label4
        // 
        this.label4.AutoSize = true;
        this.label4.Location = new System.Drawing.Point(222, 37);
        this.label4.Name = "label4";
        this.label4.Size = new System.Drawing.Size(36, 13);
        this.label4.TabIndex = 5;
        this.label4.Text = "kg/ha";
        // 
        // TotalNH4
        // 
        this.TotalNH4.Location = new System.Drawing.Point(160, 34);
        this.TotalNH4.Name = "TotalNH4";
        this.TotalNH4.Size = new System.Drawing.Size(56, 20);
        this.TotalNH4.TabIndex = 4;
        this.TotalNH4.TextChanged += new System.EventHandler(this.TotalNH4_TextChanged);
        // 
        // label3
        // 
        this.label3.AutoSize = true;
        this.label3.Location = new System.Drawing.Point(82, 37);
        this.label3.Name = "label3";
        this.label3.Size = new System.Drawing.Size(36, 13);
        this.label3.TabIndex = 3;
        this.label3.Text = "kg/ha";
        // 
        // TotalNO3
        // 
        this.TotalNO3.Location = new System.Drawing.Point(20, 34);
        this.TotalNO3.Name = "TotalNO3";
        this.TotalNO3.Size = new System.Drawing.Size(56, 20);
        this.TotalNO3.TabIndex = 2;
        this.TotalNO3.TextChanged += new System.EventHandler(this.TotalNO3_TextChanged);
        // 
        // label2
        // 
        this.label2.AutoSize = true;
        this.label2.Location = new System.Drawing.Point(157, 18);
        this.label2.Name = "label2";
        this.label2.Size = new System.Drawing.Size(59, 13);
        this.label2.TabIndex = 1;
        this.label2.Text = "Total NH4:";
        // 
        // label1
        // 
        this.label1.AutoSize = true;
        this.label1.Location = new System.Drawing.Point(17, 18);
        this.label1.Name = "label1";
        this.label1.Size = new System.Drawing.Size(59, 13);
        this.label1.TabIndex = 0;
        this.label1.Text = "Total NO3:";
        // 
        // InitNitrogenUI
        // 
        this.Controls.Add(this.panel1);
        this.Name = "InitNitrogenUI";
        this.Size = new System.Drawing.Size(908, 765);
        this.Controls.SetChildIndex(this.panel1, 0);
        this.panel1.ResumeLayout(false);
        this.LeftHandPanel.ResumeLayout(false);
        ((System.ComponentModel.ISupportInitialize)(this.Grid)).EndInit();
        ((System.ComponentModel.ISupportInitialize)(this.NitrogenGrid)).EndInit();
        this.panel3.ResumeLayout(false);
        this.panel3.PerformLayout();
        this.ResumeLayout(false);

		}
		#endregion
		#endregion

		override public void OnRefresh()
			{
			HelpText = "There are two ways of specifying initial soil nitrogen. You can either type a number for each layer (kg/ha or ppm) "
					 + " or a total NO3 / NH4 number (kg/ha only) on the last row of the grid.";

            ApsimFile.Component SoilNode = Controller.ApsimData.Find(NodePath);
            if (SoilNode != null && SoilNode.Parent != null)
                {
                SoilNode = SoilNode.Parent;
                XmlDocument Doc = new XmlDocument();
                Doc.LoadXml(SoilNode.Contents);
                SoilData = new Soil(Doc.DocumentElement);
                InitialNitrogen = new InitNitrogen(Data, SoilData);
                FarPoint.Win.Spread.InputMap InputMap = Grid.GetInputMap(FarPoint.Win.Spread.InputMapMode.WhenAncestorOfFocused);
                InputMap.Put(new FarPoint.Win.Spread.Keystroke(Keys.Delete, Keys.None),
                                FarPoint.Win.Spread.SpreadActions.ClipboardCut);
                InputMap.Put(new FarPoint.Win.Spread.Keystroke(Keys.Enter, Keys.None),
                                FarPoint.Win.Spread.SpreadActions.MoveToNextRow);
                PopulateEditBoxes(); // this will then cause TextChanged events on the edit boxes.
                PopulateGrid();
                }
			}

        private void PopulateEditBoxes()
            {
            TotalNO3.Text = InitialNitrogen.TotalNO3KgHa.ToString("f3");
            TotalNH4.Text = InitialNitrogen.TotalNH4KgHa.ToString("f3");
            }

		private void PopulateGrid()
			{
			UserChange = false;
            NitrogenGrid.ClearRange(0, 0, NitrogenGrid.RowCount, NitrogenGrid.ColumnCount, true);
            GridUtility.SetColumnAsStrings(NitrogenGrid, 0, ApsimFile.SoilComponentUtility.ToDepthStrings(InitialNitrogen.Thickness));
			GridUtility.SetColumnAsDoubles(NitrogenGrid, 1, InitialNitrogen.NO3KgHa);
			GridUtility.SetColumnAsDoubles(NitrogenGrid, 2, InitialNitrogen.NH4KgHa);
			GridUtility.SetColumnAsDoubles(NitrogenGrid, 3, InitialNitrogen.NO3);
			GridUtility.SetColumnAsDoubles(NitrogenGrid, 4, InitialNitrogen.NH4);
    		UserChange = true;
			}

		private void NitrogenGrid_CellChanged(object sender, FarPoint.Win.Spread.SheetViewEventArgs e)
			{
			if (UserChange)
				{
				UserChange = false;
                int NumLayers = GridUtility.FindFirstBlankCell(NitrogenGrid, 0);
                if (e.Column == 0)
                    {
                    // user changed depths
                        InitialNitrogen.Thickness = SoilComponentUtility.ToThickness(GridUtility.GetColumnAsStrings(NitrogenGrid, 0, NumLayers));
                    }
                else if (e.Column == 1)
					{
					// user changed layered no3 (kg/ha)
                    double[] no3 = GridUtility.GetColumnAsDoubles(NitrogenGrid, 1, NumLayers);
					InitialNitrogen.NO3KgHa = no3;
					GridUtility.SetColumnAsDoubles(NitrogenGrid, 3, InitialNitrogen.NO3);
					}
				else if (e.Column == 2)
					{
					// user changed layered nh4 (kg/ha)
                    double[] nh4 = GridUtility.GetColumnAsDoubles(NitrogenGrid, 2, NumLayers);
					InitialNitrogen.NH4KgHa = nh4;
					GridUtility.SetColumnAsDoubles(NitrogenGrid, 4, InitialNitrogen.NH4);
					}
				else if (e.Column == 3)
					{
					// user changed layered no3 (ppm)
                    double[] no3 = GridUtility.GetColumnAsDoubles(NitrogenGrid, 3, NumLayers);
					InitialNitrogen.NO3 = no3;
					GridUtility.SetColumnAsDoubles(NitrogenGrid, 1, InitialNitrogen.NO3KgHa);
					}
				else if (e.Column == 4)
					{
					// user changed layered nh4 (ppm)
                    double[] nh4 = GridUtility.GetColumnAsDoubles(NitrogenGrid, 4, NumLayers);
					InitialNitrogen.NH4 = nh4;
					GridUtility.SetColumnAsDoubles(NitrogenGrid, 2, InitialNitrogen.NH4KgHa);
					}
                PopulateEditBoxes();
				UpdateGraph();
				UserChange = true;
				}
			}
		
		private void UpdateGraph()
			{
            double[] CumThicknessMidPoints = MathUtility.Divide_Value(SoilComponentUtility.ToMidPoints(InitialNitrogen.Thickness), 10);
           
            No3KgHaLine.Add(InitialNitrogen.NO3KgHa, CumThicknessMidPoints);
            Nh4KgHaLine.Add(InitialNitrogen.NH4KgHa, CumThicknessMidPoints);
            No3PpmLine.Add(InitialNitrogen.NO3, CumThicknessMidPoints);
            Nh4PpmLine.Add(InitialNitrogen.NH4, CumThicknessMidPoints);
			}

        private void NitrogenChart_ClickLegend(object sender, MouseEventArgs e)
            // User has clicked legend
            {
            int index = NitrogenChart.Legend.Clicked(e.X, e.Y);

            if (index == 0 || index == 1)
                {
                NitrogenChart.Axes.Top.Title.Text = "Nitrogen (kg/ha)";
                No3KgHaLine.Active = true;
                Nh4KgHaLine.Active = true;
                No3PpmLine.Active = false;
                Nh4PpmLine.Active = false;
                }
            else
                {
                NitrogenChart.Axes.Top.Title.Text = "Nitrogen (ppm)";
                No3KgHaLine.Active = false;
                Nh4KgHaLine.Active = false;
                No3PpmLine.Active = true;
                Nh4PpmLine.Active = true;
                }

            }

        private void TotalNO3_TextChanged(object sender, EventArgs e)
            {
            if (TotalNO3.Text != "")
                {
                InitialNitrogen.TotalNO3KgHa = Convert.ToDouble(TotalNO3.Text);
                PopulateGrid();
                UpdateGraph();
                }
            }

        private void TotalNH4_TextChanged(object sender, EventArgs e)
            {
            if (TotalNH4.Text != "")
                {
                InitialNitrogen.TotalNH4KgHa = Convert.ToDouble(TotalNH4.Text);
                PopulateGrid();
                UpdateGraph();
                }
            }


	}
}

