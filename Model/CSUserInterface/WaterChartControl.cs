
using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Windows.Forms;

using ApsimFile;
using CSGeneral;


namespace CSUserInterface
   {
   public class WaterChartControl : System.Windows.Forms.UserControl
      {
      private System.ComponentModel.Container components = null;
      private Soil MySoil;
      private InitWater InitialWater = null;
      public Steema.TeeChart.TChart WaterChart;
      private Steema.TeeChart.Styles.Line AirDryLine;
      private Steema.TeeChart.Styles.Line SatLine;
      private Steema.TeeChart.Styles.Line DulLine;
      private Steema.TeeChart.Styles.Line LL15Line;
      private Steema.TeeChart.Tools.DragPoint DragPoint;
      private Steema.TeeChart.Styles.Line InitialWaterLine;

      public delegate void OnWaterChangeDelegate(int LayerNumber, double NewValue);
      public event OnWaterChangeDelegate OnWaterChange;

      public WaterChartControl()
         {
         // This call is required by the Windows.Forms Form Designer.
         InitializeComponent();
         }
      protected override void Dispose(bool disposing)
         {
         if (disposing)
            {
            if (components != null)
               {
               components.Dispose();
               }
            }
         base.Dispose(disposing);
         }

      #region Component Designer generated code
      /// <summary> 
      /// Required method for Designer support - do not modify 
      /// the contents of this method with the code editor.
      /// </summary>
      private void InitializeComponent()
         {
         this.WaterChart = new Steema.TeeChart.TChart();
         this.SatLine = new Steema.TeeChart.Styles.Line();
         this.DulLine = new Steema.TeeChart.Styles.Line();
         this.LL15Line = new Steema.TeeChart.Styles.Line();
         this.AirDryLine = new Steema.TeeChart.Styles.Line();
         this.InitialWaterLine = new Steema.TeeChart.Styles.Line();
         this.DragPoint = new Steema.TeeChart.Tools.DragPoint();
         this.SuspendLayout();
         // 
         // WaterChart
         // 
         // 
         // 
         // 
         this.WaterChart.Aspect.ElevationFloat = 345;
         this.WaterChart.Aspect.RotationFloat = 345;
         this.WaterChart.Aspect.View3D = false;
         // 
         // 
         // 
         // 
         // 
         // 
         this.WaterChart.Axes.Bottom.Automatic = true;
         // 
         // 
         // 
         this.WaterChart.Axes.Bottom.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
         this.WaterChart.Axes.Bottom.Grid.Visible = false;
         this.WaterChart.Axes.Bottom.Grid.ZPosition = 0;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.WaterChart.Axes.Bottom.Labels.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.WaterChart.Axes.Bottom.Labels.Shadow.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.WaterChart.Axes.Bottom.Title.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.WaterChart.Axes.Bottom.Title.Shadow.Visible = false;
         this.WaterChart.Axes.Bottom.Visible = false;
         // 
         // 
         // 
         this.WaterChart.Axes.Depth.Automatic = true;
         // 
         // 
         // 
         this.WaterChart.Axes.Depth.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
         this.WaterChart.Axes.Depth.Grid.ZPosition = 0;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.WaterChart.Axes.Depth.Labels.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.WaterChart.Axes.Depth.Labels.Shadow.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.WaterChart.Axes.Depth.Title.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.WaterChart.Axes.Depth.Title.Shadow.Visible = false;
         // 
         // 
         // 
         this.WaterChart.Axes.DepthTop.Automatic = true;
         // 
         // 
         // 
         this.WaterChart.Axes.DepthTop.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
         this.WaterChart.Axes.DepthTop.Grid.ZPosition = 0;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.WaterChart.Axes.DepthTop.Labels.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.WaterChart.Axes.DepthTop.Labels.Shadow.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.WaterChart.Axes.DepthTop.Title.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.WaterChart.Axes.DepthTop.Title.Shadow.Visible = false;
         // 
         // 
         // 
         this.WaterChart.Axes.Left.AutomaticMinimum = false;
         // 
         // 
         // 
         this.WaterChart.Axes.Left.AxisPen.Width = 1;
         // 
         // 
         // 
         this.WaterChart.Axes.Left.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
         this.WaterChart.Axes.Left.Grid.Visible = false;
         this.WaterChart.Axes.Left.Grid.ZPosition = 0;
         this.WaterChart.Axes.Left.Increment = 20;
         this.WaterChart.Axes.Left.Inverted = true;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.WaterChart.Axes.Left.Labels.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.WaterChart.Axes.Left.Labels.Shadow.Visible = false;
         this.WaterChart.Axes.Left.MaximumOffset = 2;
         this.WaterChart.Axes.Left.Minimum = 0;
         // 
         // 
         // 
         this.WaterChart.Axes.Left.MinorTicks.Visible = false;
         // 
         // 
         // 
         this.WaterChart.Axes.Left.Ticks.Length = 0;
         // 
         // 
         // 
         this.WaterChart.Axes.Left.TicksInner.Length = 5;
         // 
         // 
         // 
         this.WaterChart.Axes.Left.Title.Caption = "Depth (mm)";
         // 
         // 
         // 
         // 
         // 
         // 
         this.WaterChart.Axes.Left.Title.Font.Shadow.Visible = false;
         this.WaterChart.Axes.Left.Title.Lines = new string[] {
        "Depth (mm)"};
         // 
         // 
         // 
         this.WaterChart.Axes.Left.Title.Shadow.Visible = false;
         // 
         // 
         // 
         this.WaterChart.Axes.Right.Automatic = true;
         // 
         // 
         // 
         this.WaterChart.Axes.Right.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
         this.WaterChart.Axes.Right.Grid.ZPosition = 0;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.WaterChart.Axes.Right.Labels.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.WaterChart.Axes.Right.Labels.Shadow.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.WaterChart.Axes.Right.Title.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.WaterChart.Axes.Right.Title.Shadow.Visible = false;
         this.WaterChart.Axes.Right.Visible = false;
         // 
         // 
         // 
         this.WaterChart.Axes.Top.AutomaticMinimum = false;
         // 
         // 
         // 
         this.WaterChart.Axes.Top.AxisPen.Width = 1;
         // 
         // 
         // 
         this.WaterChart.Axes.Top.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
         this.WaterChart.Axes.Top.Grid.Visible = false;
         this.WaterChart.Axes.Top.Grid.ZPosition = 0;
         this.WaterChart.Axes.Top.Increment = 5;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.WaterChart.Axes.Top.Labels.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.WaterChart.Axes.Top.Labels.Shadow.Visible = false;
         this.WaterChart.Axes.Top.MaximumOffset = 2;
         this.WaterChart.Axes.Top.Minimum = 0;
         // 
         // 
         // 
         this.WaterChart.Axes.Top.MinorTicks.Visible = false;
         // 
         // 
         // 
         this.WaterChart.Axes.Top.Ticks.Length = 0;
         this.WaterChart.Axes.Top.Ticks.Visible = false;
         // 
         // 
         // 
         this.WaterChart.Axes.Top.TicksInner.Length = 5;
         // 
         // 
         // 
         this.WaterChart.Axes.Top.Title.Caption = "Volumetric Water (%)";
         // 
         // 
         // 
         // 
         // 
         // 
         this.WaterChart.Axes.Top.Title.Font.Shadow.Visible = false;
         this.WaterChart.Axes.Top.Title.Lines = new string[] {
        "Volumetric Water (%)"};
         // 
         // 
         // 
         this.WaterChart.Axes.Top.Title.Shadow.Visible = false;
         this.WaterChart.Cursor = System.Windows.Forms.Cursors.Default;
         this.WaterChart.Dock = System.Windows.Forms.DockStyle.Fill;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.WaterChart.Footer.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.WaterChart.Footer.Shadow.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.WaterChart.Header.Font.Shadow.Visible = false;
         this.WaterChart.Header.Lines = new string[] {
        ""};
         // 
         // 
         // 
         this.WaterChart.Header.Shadow.Visible = false;
         // 
         // 
         // 
         this.WaterChart.Legend.CheckBoxes = true;
         // 
         // 
         // 
         // 
         // 
         // 
         this.WaterChart.Legend.Font.Shadow.Visible = false;
         this.WaterChart.Legend.LegendStyle = Steema.TeeChart.LegendStyles.Series;
         // 
         // 
         // 
         this.WaterChart.Legend.Pen.Visible = false;
         // 
         // 
         // 
         this.WaterChart.Legend.Shadow.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         this.WaterChart.Legend.Title.Font.Bold = true;
         // 
         // 
         // 
         this.WaterChart.Legend.Title.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.WaterChart.Legend.Title.Pen.Visible = false;
         // 
         // 
         // 
         this.WaterChart.Legend.Title.Shadow.Visible = false;
         this.WaterChart.Location = new System.Drawing.Point(0, 0);
         this.WaterChart.Name = "WaterChart";
         // 
         // 
         // 
         // 
         // 
         // 
         this.WaterChart.Panel.Bevel.Outer = Steema.TeeChart.Drawing.BevelStyles.None;
         // 
         // 
         // 
         this.WaterChart.Panel.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
         // 
         // 
         // 
         this.WaterChart.Panel.ImageBevel.Width = 1;
         this.WaterChart.Panel.MarginBottom = 8;
         this.WaterChart.Panel.MarginLeft = 7;
         this.WaterChart.Panel.MarginRight = 7;
         // 
         // 
         // 
         this.WaterChart.Panel.Shadow.Visible = false;
         this.WaterChart.Series.Add(this.SatLine);
         this.WaterChart.Series.Add(this.DulLine);
         this.WaterChart.Series.Add(this.LL15Line);
         this.WaterChart.Series.Add(this.AirDryLine);
         this.WaterChart.Series.Add(this.InitialWaterLine);
         this.WaterChart.Size = new System.Drawing.Size(648, 600);
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.WaterChart.SubFooter.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.WaterChart.SubFooter.Shadow.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.WaterChart.SubHeader.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.WaterChart.SubHeader.Shadow.Visible = false;
         this.WaterChart.TabIndex = 25;
         this.WaterChart.Tools.Add(this.DragPoint);
         // 
         // 
         // 
         // 
         // 
         // 
         this.WaterChart.Walls.Back.AutoHide = false;
         // 
         // 
         // 
         this.WaterChart.Walls.Back.Shadow.Visible = false;
         // 
         // 
         // 
         this.WaterChart.Walls.Bottom.AutoHide = false;
         // 
         // 
         // 
         this.WaterChart.Walls.Bottom.Shadow.Visible = false;
         // 
         // 
         // 
         this.WaterChart.Walls.Left.AutoHide = false;
         // 
         // 
         // 
         this.WaterChart.Walls.Left.Shadow.Visible = false;
         // 
         // 
         // 
         this.WaterChart.Walls.Right.AutoHide = false;
         // 
         // 
         // 
         this.WaterChart.Walls.Right.Shadow.Visible = false;
         this.WaterChart.Walls.Visible = false;
         this.WaterChart.ClickLegend += new System.Windows.Forms.MouseEventHandler(this.WaterChart_ClickLegend);
         // 
         // SatLine
         // 
         // 
         // 
         // 
         this.SatLine.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(255)))));
         this.SatLine.HorizAxis = Steema.TeeChart.Styles.HorizontalAxis.Top;
         // 
         // 
         // 
         this.SatLine.LinePen.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(153)))));
         this.SatLine.LinePen.Style = System.Drawing.Drawing2D.DashStyle.Dash;
         // 
         // 
         // 
         // 
         // 
         // 
         this.SatLine.Marks.Callout.ArrowHead = Steema.TeeChart.Styles.ArrowHeadStyles.None;
         this.SatLine.Marks.Callout.ArrowHeadSize = 8;
         // 
         // 
         // 
         this.SatLine.Marks.Callout.Brush.Color = System.Drawing.Color.Black;
         this.SatLine.Marks.Callout.Distance = 0;
         this.SatLine.Marks.Callout.Draw3D = false;
         this.SatLine.Marks.Callout.Length = 10;
         this.SatLine.Marks.Callout.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
         // 
         // 
         // 
         // 
         // 
         // 
         this.SatLine.Marks.Font.Shadow.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         this.SatLine.Pointer.Brush.Color = System.Drawing.Color.Green;
         this.SatLine.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
         this.SatLine.Title = "Saturation";
         // 
         // 
         // 
         this.SatLine.XValues.DataMember = "X";
         this.SatLine.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending;
         // 
         // 
         // 
         this.SatLine.YValues.DataMember = "Y";
         // 
         // DulLine
         // 
         // 
         // 
         // 
         this.DulLine.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(255)))));
         this.DulLine.HorizAxis = Steema.TeeChart.Styles.HorizontalAxis.Top;
         // 
         // 
         // 
         this.DulLine.LinePen.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(153)))));
         // 
         // 
         // 
         // 
         // 
         // 
         this.DulLine.Marks.Callout.ArrowHead = Steema.TeeChart.Styles.ArrowHeadStyles.None;
         this.DulLine.Marks.Callout.ArrowHeadSize = 8;
         // 
         // 
         // 
         this.DulLine.Marks.Callout.Brush.Color = System.Drawing.Color.Black;
         this.DulLine.Marks.Callout.Distance = 0;
         this.DulLine.Marks.Callout.Draw3D = false;
         this.DulLine.Marks.Callout.Length = 10;
         this.DulLine.Marks.Callout.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
         // 
         // 
         // 
         // 
         // 
         // 
         this.DulLine.Marks.Font.Shadow.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         this.DulLine.Pointer.Brush.Color = System.Drawing.Color.Yellow;
         this.DulLine.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
         this.DulLine.Title = "Drained Upper Limit";
         // 
         // 
         // 
         this.DulLine.XValues.DataMember = "X";
         this.DulLine.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending;
         // 
         // 
         // 
         this.DulLine.YValues.DataMember = "Y";
         // 
         // LL15Line
         // 
         // 
         // 
         // 
         this.LL15Line.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(192)))), ((int)(((byte)(128)))));
         this.LL15Line.HorizAxis = Steema.TeeChart.Styles.HorizontalAxis.Top;
         // 
         // 
         // 
         this.LL15Line.LinePen.Color = System.Drawing.Color.FromArgb(((int)(((byte)(153)))), ((int)(((byte)(115)))), ((int)(((byte)(77)))));
         // 
         // 
         // 
         // 
         // 
         // 
         this.LL15Line.Marks.Callout.ArrowHead = Steema.TeeChart.Styles.ArrowHeadStyles.None;
         this.LL15Line.Marks.Callout.ArrowHeadSize = 8;
         // 
         // 
         // 
         this.LL15Line.Marks.Callout.Brush.Color = System.Drawing.Color.Black;
         this.LL15Line.Marks.Callout.Distance = 0;
         this.LL15Line.Marks.Callout.Draw3D = false;
         this.LL15Line.Marks.Callout.Length = 10;
         this.LL15Line.Marks.Callout.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
         // 
         // 
         // 
         // 
         // 
         // 
         this.LL15Line.Marks.Font.Shadow.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         this.LL15Line.Pointer.Brush.Color = System.Drawing.Color.Yellow;
         this.LL15Line.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
         this.LL15Line.Title = "Lower Limit 15bar";
         // 
         // 
         // 
         this.LL15Line.XValues.DataMember = "X";
         this.LL15Line.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending;
         // 
         // 
         // 
         this.LL15Line.YValues.DataMember = "Y";
         // 
         // AirDryLine
         // 
         // 
         // 
         // 
         this.AirDryLine.Brush.Color = System.Drawing.Color.Red;
         this.AirDryLine.HorizAxis = Steema.TeeChart.Styles.HorizontalAxis.Top;
         // 
         // 
         // 
         this.AirDryLine.LinePen.Color = System.Drawing.Color.FromArgb(((int)(((byte)(153)))), ((int)(((byte)(0)))), ((int)(((byte)(0)))));
         this.AirDryLine.LinePen.Style = System.Drawing.Drawing2D.DashStyle.Dash;
         // 
         // 
         // 
         // 
         // 
         // 
         this.AirDryLine.Marks.Callout.ArrowHead = Steema.TeeChart.Styles.ArrowHeadStyles.None;
         this.AirDryLine.Marks.Callout.ArrowHeadSize = 8;
         // 
         // 
         // 
         this.AirDryLine.Marks.Callout.Brush.Color = System.Drawing.Color.Black;
         this.AirDryLine.Marks.Callout.Distance = 0;
         this.AirDryLine.Marks.Callout.Draw3D = false;
         this.AirDryLine.Marks.Callout.Length = 10;
         this.AirDryLine.Marks.Callout.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
         // 
         // 
         // 
         // 
         // 
         // 
         this.AirDryLine.Marks.Font.Shadow.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         this.AirDryLine.Pointer.Brush.Color = System.Drawing.Color.Red;
         this.AirDryLine.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
         this.AirDryLine.Title = "AirDry";
         // 
         // 
         // 
         this.AirDryLine.XValues.DataMember = "X";
         this.AirDryLine.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending;
         // 
         // 
         // 
         this.AirDryLine.YValues.DataMember = "Y";
         // 
         // InitialWaterLine
         // 
         // 
         // 
         // 
         this.InitialWaterLine.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
         this.InitialWaterLine.HorizAxis = Steema.TeeChart.Styles.HorizontalAxis.Top;
         // 
         // 
         // 
         this.InitialWaterLine.LinePen.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(153)))), ((int)(((byte)(153)))));
         this.InitialWaterLine.LinePen.Width = 2;
         // 
         // 
         // 
         // 
         // 
         // 
         this.InitialWaterLine.Marks.Callout.ArrowHead = Steema.TeeChart.Styles.ArrowHeadStyles.None;
         this.InitialWaterLine.Marks.Callout.ArrowHeadSize = 8;
         // 
         // 
         // 
         this.InitialWaterLine.Marks.Callout.Brush.Color = System.Drawing.Color.Black;
         this.InitialWaterLine.Marks.Callout.Distance = 0;
         this.InitialWaterLine.Marks.Callout.Draw3D = false;
         this.InitialWaterLine.Marks.Callout.Length = 10;
         this.InitialWaterLine.Marks.Callout.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
         // 
         // 
         // 
         // 
         // 
         // 
         this.InitialWaterLine.Marks.Font.Shadow.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         this.InitialWaterLine.Pointer.Brush.Color = System.Drawing.Color.Green;
         this.InitialWaterLine.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
         this.InitialWaterLine.Pointer.Visible = true;
         this.InitialWaterLine.Title = "Initial water";
         // 
         // 
         // 
         this.InitialWaterLine.XValues.DataMember = "X";
         this.InitialWaterLine.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending;
         // 
         // 
         // 
         this.InitialWaterLine.YValues.DataMember = "Y";
         // 
         // DragPoint
         // 
         this.DragPoint.Cursor = System.Windows.Forms.Cursors.SizeWE;
         this.DragPoint.Series = this.InitialWaterLine;
         this.DragPoint.Style = Steema.TeeChart.Tools.DragPointStyles.X;
         this.DragPoint.Drag += new Steema.TeeChart.Tools.DragPointEventHandler(this.OnDragInitWater);
         // 
         // WaterChartControl
         // 
         this.Controls.Add(this.WaterChart);
         this.Name = "WaterChartControl";
         this.Size = new System.Drawing.Size(648, 600);
         this.ResumeLayout(false);

         }
      #endregion


      public Soil LinkedSoil
         {
         get { return MySoil; }
         set
            {
            MySoil = value;
            RefreshView();
            }
         }
      public InitWater LinkedSoilWater
         {
         get { return InitialWater; }
         set
            {
            InitialWater = value;
            RefreshView();
            }
         }

      // -------------------
      // Refresh            
      // -------------------
      public void RefreshView()
         {
         if (MySoil != null)
            PopulateWaterChart();
         }


      // ---------------------------
      // populate water chart.
      // ---------------------------
      private void PopulateWaterChart()
         {
         double[] CumThicknessMidPoints = MathUtility.Divide_Value(SoilComponentUtility.ToMidPoints(MySoil.Thickness), 10);
         if (CumThicknessMidPoints.Length == 0)
            return;
         if ((MySoil.SAT[MySoil.SAT.Length - 1] <= 1) &&
             (MySoil.DUL[MySoil.DUL.Length - 1] <= 1) &&
             (MySoil.LL15[MySoil.LL15.Length - 1] <= 1) &&
             (MySoil.Airdry[MySoil.Airdry.Length - 1] <= 1))
            {
            SatLine.Add(MathUtility.Multiply_Value(MySoil.SAT, 100), CumThicknessMidPoints);
            DulLine.Add(MathUtility.Multiply_Value(MySoil.DUL, 100), CumThicknessMidPoints);
            LL15Line.Add(MathUtility.Multiply_Value(MySoil.LL15, 100), CumThicknessMidPoints);
            AirDryLine.Add(MathUtility.Multiply_Value(MySoil.Airdry, 100), CumThicknessMidPoints);
            InitialWaterLine.Active = (InitialWater != null);
            InitialWaterLine.ShowInLegend = InitialWaterLine.Active;
            if (InitialWaterLine.Active)
               InitialWaterLine.Add(MathUtility.Multiply_Value(InitialWater.SW, 100), CumThicknessMidPoints);

            // remove existing crops.
            while (WaterChart.Series.Count > 5)
               WaterChart.Series.Remove(WaterChart.Series[5]);

            // put crops on graph.
            List<string> SelectedCrops = Configuration.Instance.Settings("CropsOnGraph");
            Color[] Colours = { Color.Green, Color.GreenYellow, Color.Pink, Color.SaddleBrown, Color.Silver };
            int ColourIndex = 0;
            foreach (string Crop in MySoil.Crops)
               {
               Steema.TeeChart.Styles.Line CropSeries = new Steema.TeeChart.Styles.Line();
               WaterChart.Series.Add(CropSeries);
               CropSeries.HorizAxis = Steema.TeeChart.Styles.HorizontalAxis.Top;
               if (InitialWater != null)
                  {
                  double PAW = MathUtility.Sum(InitialWater.PAW(Crop));
                  CropSeries.Title = Crop + " ll (PAW= " + PAW.ToString("f0") + ")";
                  }
               else
                  CropSeries.Title = Crop + " ll";
               CropSeries.Color = Colours[ColourIndex];
               CropSeries.LinePen.Width = 2;
               CropSeries.Add(MathUtility.Multiply_Value(MySoil.LL(Crop), 100), CumThicknessMidPoints);
               CropSeries.Active = (SelectedCrops.IndexOf(Crop) != -1);

               ColourIndex++;
               if (ColourIndex == Colours.Length) ColourIndex = 0;
               }
            }
         }



      // -----------------------------------------------
      // User has clicked on legend - save ticked crops.
      // -----------------------------------------------
      private void WaterChart_ClickLegend(object sender, MouseEventArgs e)
         {
         AirDryLine.Active = true;
         SatLine.Active = true;
         DulLine.Active = true;
         LL15Line.Active = true;
         InitialWaterLine.Active = true;
         List<string> TickedCrops = new List<string>();
         for (int s = 5; s != WaterChart.Series.Count; s++)
            {
            if (WaterChart.Series[s].Active)
               TickedCrops.Add(MySoil.Crops[s - 5]);
            }
         Configuration.Instance.SetSettings("CropsOnGraph", TickedCrops);
         }

      private void OnDragInitWater(Steema.TeeChart.Tools.DragPoint sender, int index)
         {
         // -----------------------------------------------
         // User is dragging a initwater point - send out
         // an event so that our parent form can subscribe
         // to the event and update their table.
         // -----------------------------------------------
         Steema.TeeChart.Tools.DragPoint dp = (Steema.TeeChart.Tools.DragPoint)sender;
         double NewValue = InitialWaterLine.XValues[index];
         if (OnWaterChange != null)
            OnWaterChange.Invoke(index, NewValue);
         }




      }
   }
