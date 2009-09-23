using CSUserInterface;
using UIBits;

namespace ApsimRun
   {
   partial class SimulationRunnerForm
      {
      /// <summary>
      /// Required designer variable.
      /// </summary>
      private System.ComponentModel.IContainer components = null;

      /// <summary>
      /// Clean up any resources being used.
      /// </summary>
      /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
      protected override void Dispose(bool disposing)
         {
         if (disposing && (components != null))
            {
            components.Dispose();
            }
         base.Dispose(disposing);
         }

      #region Windows Form Designer generated code

      /// <summary>
      /// Required method for Designer support - do not modify
      /// the contents of this method with the code editor.
      /// </summary>
      private void InitializeComponent()
         {
         this.components = new System.ComponentModel.Container();
         System.Windows.Forms.GroupBox groupBox1;
         System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(SimulationRunnerForm));
         System.Windows.Forms.Label label1;
         System.Windows.Forms.ListViewItem listViewItem1 = new System.Windows.Forms.ListViewItem("          ");
         this.PerformanceChart = new Steema.TeeChart.TChart();
         this.PerformanceSeries = new Steema.TeeChart.Styles.Area();
         this.ImageList = new System.Windows.Forms.ImageList(this.components);
         this.Timer1 = new System.Windows.Forms.Timer(this.components);
         this.groupBox2 = new System.Windows.Forms.GroupBox();
         this.groupBox3 = new System.Windows.Forms.GroupBox();
         this.ProgressBar = new UIBits.VerticalProgressBar();
         this.NumberWithWarnings = new System.Windows.Forms.Label();
         this.NumberWithErrors = new System.Windows.Forms.Label();
         this.Completed = new System.Windows.Forms.Label();
         this.Total = new System.Windows.Forms.Label();
         this.label5 = new System.Windows.Forms.Label();
         this.label4 = new System.Windows.Forms.Label();
         this.label3 = new System.Windows.Forms.Label();
         this.label2 = new System.Windows.Forms.Label();
         this.NumCPUs = new System.Windows.Forms.NumericUpDown();
         this.ToolStrip = new System.Windows.Forms.ToolStrip();
         this.StopButton = new System.Windows.Forms.ToolStripButton();
         this.PauseButton = new System.Windows.Forms.ToolStripButton();
         this.ShowDetailButton = new System.Windows.Forms.LinkLabel();
         this.SimulationList = new System.Windows.Forms.ListView();
         this.columnHeader1 = new System.Windows.Forms.ColumnHeader();
         groupBox1 = new System.Windows.Forms.GroupBox();
         label1 = new System.Windows.Forms.Label();
         groupBox1.SuspendLayout();
         this.groupBox2.SuspendLayout();
         this.groupBox3.SuspendLayout();
         ((System.ComponentModel.ISupportInitialize)(this.NumCPUs)).BeginInit();
         this.ToolStrip.SuspendLayout();
         this.SuspendLayout();
         // 
         // groupBox1
         // 
         groupBox1.Controls.Add(this.PerformanceChart);
         groupBox1.Location = new System.Drawing.Point(255, 36);
         groupBox1.Name = "groupBox1";
         groupBox1.Size = new System.Drawing.Size(120, 111);
         groupBox1.TabIndex = 12;
         groupBox1.TabStop = false;
         groupBox1.Text = "CPU usage";
         // 
         // PerformanceChart
         // 
         this.PerformanceChart.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                     | System.Windows.Forms.AnchorStyles.Left)
                     | System.Windows.Forms.AnchorStyles.Right)));
         // 
         // 
         // 
         this.PerformanceChart.Aspect.ElevationFloat = 345;
         this.PerformanceChart.Aspect.RotationFloat = 345;
         this.PerformanceChart.Aspect.View3D = false;
         // 
         // 
         // 
         // 
         // 
         // 
         this.PerformanceChart.Axes.Bottom.AutomaticMaximum = false;
         this.PerformanceChart.Axes.Bottom.AutomaticMinimum = false;
         // 
         // 
         // 
         this.PerformanceChart.Axes.Bottom.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
         this.PerformanceChart.Axes.Bottom.Grid.ZPosition = 0;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.PerformanceChart.Axes.Bottom.Labels.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.PerformanceChart.Axes.Bottom.Labels.Shadow.Visible = false;
         this.PerformanceChart.Axes.Bottom.Maximum = 10;
         this.PerformanceChart.Axes.Bottom.Minimum = 0;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.PerformanceChart.Axes.Bottom.Title.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.PerformanceChart.Axes.Bottom.Title.Shadow.Visible = false;
         this.PerformanceChart.Axes.Bottom.Visible = false;
         // 
         // 
         // 
         this.PerformanceChart.Axes.Depth.Automatic = true;
         // 
         // 
         // 
         this.PerformanceChart.Axes.Depth.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
         this.PerformanceChart.Axes.Depth.Grid.ZPosition = 0;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.PerformanceChart.Axes.Depth.Labels.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.PerformanceChart.Axes.Depth.Labels.Shadow.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.PerformanceChart.Axes.Depth.Title.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.PerformanceChart.Axes.Depth.Title.Shadow.Visible = false;
         // 
         // 
         // 
         this.PerformanceChart.Axes.DepthTop.Automatic = true;
         // 
         // 
         // 
         this.PerformanceChart.Axes.DepthTop.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
         this.PerformanceChart.Axes.DepthTop.Grid.ZPosition = 0;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.PerformanceChart.Axes.DepthTop.Labels.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.PerformanceChart.Axes.DepthTop.Labels.Shadow.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.PerformanceChart.Axes.DepthTop.Title.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.PerformanceChart.Axes.DepthTop.Title.Shadow.Visible = false;
         // 
         // 
         // 
         this.PerformanceChart.Axes.Left.AutomaticMaximum = false;
         this.PerformanceChart.Axes.Left.AutomaticMinimum = false;
         // 
         // 
         // 
         this.PerformanceChart.Axes.Left.AxisPen.Color = System.Drawing.Color.FromArgb(((int)(((byte)(224)))), ((int)(((byte)(224)))), ((int)(((byte)(224)))));
         this.PerformanceChart.Axes.Left.AxisPen.Visible = false;
         this.PerformanceChart.Axes.Left.AxisPen.Width = 1;
         // 
         // 
         // 
         this.PerformanceChart.Axes.Left.Grid.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(128)))), ((int)(((byte)(0)))));
         this.PerformanceChart.Axes.Left.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
         this.PerformanceChart.Axes.Left.Grid.Transparency = 25;
         this.PerformanceChart.Axes.Left.Grid.ZPosition = 0;
         this.PerformanceChart.Axes.Left.Increment = 25;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.PerformanceChart.Axes.Left.Labels.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.PerformanceChart.Axes.Left.Labels.Shadow.Visible = false;
         this.PerformanceChart.Axes.Left.Labels.ValueFormat = " ";
         this.PerformanceChart.Axes.Left.Maximum = 100;
         this.PerformanceChart.Axes.Left.Minimum = 0;
         // 
         // 
         // 
         this.PerformanceChart.Axes.Left.MinorTicks.Visible = false;
         // 
         // 
         // 
         this.PerformanceChart.Axes.Left.Ticks.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(128)))), ((int)(((byte)(0)))));
         this.PerformanceChart.Axes.Left.Ticks.Length = 0;
         this.PerformanceChart.Axes.Left.Ticks.Visible = false;
         // 
         // 
         // 
         this.PerformanceChart.Axes.Left.TicksInner.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.PerformanceChart.Axes.Left.Title.Font.Shadow.Visible = false;
         this.PerformanceChart.Axes.Left.Title.Lines = new string[] {
        ""};
         // 
         // 
         // 
         this.PerformanceChart.Axes.Left.Title.Shadow.Visible = false;
         // 
         // 
         // 
         this.PerformanceChart.Axes.Right.Automatic = true;
         // 
         // 
         // 
         this.PerformanceChart.Axes.Right.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
         this.PerformanceChart.Axes.Right.Grid.ZPosition = 0;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.PerformanceChart.Axes.Right.Labels.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.PerformanceChart.Axes.Right.Labels.Shadow.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.PerformanceChart.Axes.Right.Title.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.PerformanceChart.Axes.Right.Title.Shadow.Visible = false;
         this.PerformanceChart.Axes.Right.Visible = false;
         // 
         // 
         // 
         this.PerformanceChart.Axes.Top.Automatic = true;
         // 
         // 
         // 
         this.PerformanceChart.Axes.Top.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
         this.PerformanceChart.Axes.Top.Grid.ZPosition = 0;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.PerformanceChart.Axes.Top.Labels.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.PerformanceChart.Axes.Top.Labels.Shadow.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.PerformanceChart.Axes.Top.Title.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.PerformanceChart.Axes.Top.Title.Shadow.Visible = false;
         this.PerformanceChart.Axes.Top.Visible = false;
         this.PerformanceChart.Cursor = System.Windows.Forms.Cursors.Default;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.PerformanceChart.Footer.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.PerformanceChart.Footer.Shadow.Visible = false;
         // 
         // 
         // 
         this.PerformanceChart.Header.AdjustFrame = false;
         // 
         // 
         // 
         // 
         // 
         // 
         this.PerformanceChart.Header.Font.Shadow.Visible = false;
         this.PerformanceChart.Header.Lines = new string[] {
        ""};
         // 
         // 
         // 
         this.PerformanceChart.Header.Shadow.Visible = false;
         this.PerformanceChart.Header.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.PerformanceChart.Legend.Font.Shadow.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         this.PerformanceChart.Legend.Title.Font.Bold = true;
         // 
         // 
         // 
         this.PerformanceChart.Legend.Title.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.PerformanceChart.Legend.Title.Pen.Visible = false;
         // 
         // 
         // 
         this.PerformanceChart.Legend.Title.Shadow.Visible = false;
         this.PerformanceChart.Legend.Title.Visible = false;
         this.PerformanceChart.Legend.Visible = false;
         this.PerformanceChart.Location = new System.Drawing.Point(7, 16);
         this.PerformanceChart.Name = "PerformanceChart";
         // 
         // 
         // 
         // 
         // 
         // 
         this.PerformanceChart.Panel.Bevel.Outer = Steema.TeeChart.Drawing.BevelStyles.None;
         // 
         // 
         // 
         this.PerformanceChart.Panel.ImageBevel.Width = 1;
         // 
         // 
         // 
         this.PerformanceChart.Panel.Shadow.Visible = false;
         // 
         // 
         // 
         this.PerformanceChart.Panning.Allow = Steema.TeeChart.ScrollModes.None;
         this.PerformanceChart.Series.Add(this.PerformanceSeries);
         this.PerformanceChart.Size = new System.Drawing.Size(107, 85);
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.PerformanceChart.SubFooter.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.PerformanceChart.SubFooter.Shadow.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.PerformanceChart.SubHeader.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.PerformanceChart.SubHeader.Shadow.Visible = false;
         this.PerformanceChart.TabIndex = 11;
         // 
         // 
         // 
         // 
         // 
         // 
         this.PerformanceChart.Walls.Back.AutoHide = false;
         // 
         // 
         // 
         this.PerformanceChart.Walls.Back.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(128)))), ((int)(((byte)(0)))));
         // 
         // 
         // 
         this.PerformanceChart.Walls.Back.Pen.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(128)))), ((int)(((byte)(0)))));
         // 
         // 
         // 
         this.PerformanceChart.Walls.Back.Shadow.Visible = false;
         // 
         // 
         // 
         this.PerformanceChart.Walls.Bottom.AutoHide = false;
         // 
         // 
         // 
         this.PerformanceChart.Walls.Bottom.Shadow.Visible = false;
         this.PerformanceChart.Walls.Bottom.Visible = false;
         // 
         // 
         // 
         this.PerformanceChart.Walls.Left.ApplyDark = false;
         this.PerformanceChart.Walls.Left.AutoHide = false;
         // 
         // 
         // 
         this.PerformanceChart.Walls.Left.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(3)))), ((int)(((byte)(192)))), ((int)(((byte)(255)))), ((int)(((byte)(192)))));
         // 
         // 
         // 
         this.PerformanceChart.Walls.Left.Gradient.Transparency = 99;
         // 
         // 
         // 
         this.PerformanceChart.Walls.Left.Pen.Color = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(255)))), ((int)(((byte)(128)))));
         // 
         // 
         // 
         this.PerformanceChart.Walls.Left.Shadow.Visible = false;
         this.PerformanceChart.Walls.Left.Transparent = true;
         this.PerformanceChart.Walls.Left.Visible = false;
         // 
         // 
         // 
         this.PerformanceChart.Walls.Right.AutoHide = false;
         // 
         // 
         // 
         this.PerformanceChart.Walls.Right.Shadow.Visible = false;
         // 
         // 
         // 
         this.PerformanceChart.Zoom.Allow = false;
         // 
         // PerformanceSeries
         // 
         // 
         // 
         // 
         this.PerformanceSeries.AreaBrush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(192)))), ((int)(((byte)(0)))));
         // 
         // 
         // 
         this.PerformanceSeries.AreaLines.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(115)))), ((int)(((byte)(0)))));
         this.PerformanceSeries.AreaLines.Visible = false;
         // 
         // 
         // 
         this.PerformanceSeries.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(192)))), ((int)(((byte)(0)))));
         // 
         // 
         // 
         this.PerformanceSeries.LinePen.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(115)))), ((int)(((byte)(0)))));
         // 
         // 
         // 
         // 
         // 
         // 
         this.PerformanceSeries.Marks.Callout.ArrowHead = Steema.TeeChart.Styles.ArrowHeadStyles.None;
         this.PerformanceSeries.Marks.Callout.ArrowHeadSize = 8;
         // 
         // 
         // 
         this.PerformanceSeries.Marks.Callout.Brush.Color = System.Drawing.Color.Black;
         this.PerformanceSeries.Marks.Callout.Distance = 0;
         this.PerformanceSeries.Marks.Callout.Draw3D = false;
         this.PerformanceSeries.Marks.Callout.Length = 10;
         this.PerformanceSeries.Marks.Callout.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
         // 
         // 
         // 
         // 
         // 
         // 
         this.PerformanceSeries.Marks.Font.Shadow.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         this.PerformanceSeries.Pointer.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(192)))), ((int)(((byte)(0)))));
         this.PerformanceSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
         this.PerformanceSeries.Title = "area1";
         // 
         // 
         // 
         this.PerformanceSeries.XValues.DataMember = "X";
         this.PerformanceSeries.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending;
         // 
         // 
         // 
         this.PerformanceSeries.YValues.DataMember = "Y";
         // 
         // label1
         // 
         label1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
         label1.AutoSize = true;
         label1.Location = new System.Drawing.Point(255, 9);
         label1.Name = "label1";
         label1.Size = new System.Drawing.Size(69, 13);
         label1.TabIndex = 16;
         label1.Text = "CPUs to use:";
         label1.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
         // 
         // ImageList
         // 
         this.ImageList.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("ImageList.ImageStream")));
         this.ImageList.TransparentColor = System.Drawing.Color.Transparent;
         this.ImageList.Images.SetKeyName(0, "media_play.png");
         this.ImageList.Images.SetKeyName(1, "media_stop.png");
         this.ImageList.Images.SetKeyName(2, "media_pause.png");
         this.ImageList.Images.SetKeyName(3, "error.png");
         this.ImageList.Images.SetKeyName(4, "warning.png");
         this.ImageList.Images.SetKeyName(5, "check2.png");
         // 
         // Timer1
         // 
         this.Timer1.Interval = 1000;
         this.Timer1.Tick += new System.EventHandler(this.OnTimerTick);
         // 
         // groupBox2
         // 
         this.groupBox2.Controls.Add(this.groupBox3);
         this.groupBox2.Controls.Add(this.NumberWithWarnings);
         this.groupBox2.Controls.Add(this.NumberWithErrors);
         this.groupBox2.Controls.Add(this.Completed);
         this.groupBox2.Controls.Add(this.Total);
         this.groupBox2.Controls.Add(this.label5);
         this.groupBox2.Controls.Add(this.label4);
         this.groupBox2.Controls.Add(this.label3);
         this.groupBox2.Controls.Add(this.label2);
         this.groupBox2.Location = new System.Drawing.Point(9, 36);
         this.groupBox2.Name = "groupBox2";
         this.groupBox2.Size = new System.Drawing.Size(211, 111);
         this.groupBox2.TabIndex = 15;
         this.groupBox2.TabStop = false;
         this.groupBox2.Text = "Summary";
         // 
         // groupBox3
         // 
         this.groupBox3.Controls.Add(this.ProgressBar);
         this.groupBox3.Location = new System.Drawing.Point(183, 9);
         this.groupBox3.Name = "groupBox3";
         this.groupBox3.Size = new System.Drawing.Size(22, 96);
         this.groupBox3.TabIndex = 24;
         this.groupBox3.TabStop = false;
         // 
         // ProgressBar
         // 
         this.ProgressBar.BorderStyle = UIBits.BorderStyles.None;
         this.ProgressBar.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(192)))), ((int)(((byte)(0)))));
         this.ProgressBar.Location = new System.Drawing.Point(5, 9);
         this.ProgressBar.Maximum = 100;
         this.ProgressBar.Minimum = 0;
         this.ProgressBar.Name = "ProgressBar";
         this.ProgressBar.Size = new System.Drawing.Size(11, 85);
         this.ProgressBar.Step = 1;
         this.ProgressBar.Style = UIBits.Styles.Solid;
         this.ProgressBar.TabIndex = 8;
         this.ProgressBar.Value = 0;
         // 
         // NumberWithWarnings
         // 
         this.NumberWithWarnings.AutoSize = true;
         this.NumberWithWarnings.Location = new System.Drawing.Point(151, 84);
         this.NumberWithWarnings.Name = "NumberWithWarnings";
         this.NumberWithWarnings.Size = new System.Drawing.Size(13, 13);
         this.NumberWithWarnings.TabIndex = 22;
         this.NumberWithWarnings.Text = "0";
         this.NumberWithWarnings.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
         // 
         // NumberWithErrors
         // 
         this.NumberWithErrors.AutoSize = true;
         this.NumberWithErrors.Location = new System.Drawing.Point(151, 60);
         this.NumberWithErrors.Name = "NumberWithErrors";
         this.NumberWithErrors.Size = new System.Drawing.Size(13, 13);
         this.NumberWithErrors.TabIndex = 23;
         this.NumberWithErrors.Text = "0";
         this.NumberWithErrors.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
         // 
         // Completed
         // 
         this.Completed.AutoSize = true;
         this.Completed.Location = new System.Drawing.Point(151, 38);
         this.Completed.Name = "Completed";
         this.Completed.Size = new System.Drawing.Size(13, 13);
         this.Completed.TabIndex = 22;
         this.Completed.Text = "0";
         this.Completed.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
         // 
         // Total
         // 
         this.Total.AutoSize = true;
         this.Total.Location = new System.Drawing.Point(151, 16);
         this.Total.Name = "Total";
         this.Total.Size = new System.Drawing.Size(13, 13);
         this.Total.TabIndex = 21;
         this.Total.Text = "0";
         this.Total.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
         // 
         // label5
         // 
         this.label5.AutoSize = true;
         this.label5.Location = new System.Drawing.Point(8, 84);
         this.label5.Name = "label5";
         this.label5.Size = new System.Drawing.Size(114, 13);
         this.label5.TabIndex = 20;
         this.label5.Text = "Number with warnings:";
         // 
         // label4
         // 
         this.label4.AutoSize = true;
         this.label4.Location = new System.Drawing.Point(8, 60);
         this.label4.Name = "label4";
         this.label4.Size = new System.Drawing.Size(98, 13);
         this.label4.TabIndex = 19;
         this.label4.Text = "Number with errors:";
         // 
         // label3
         // 
         this.label3.AutoSize = true;
         this.label3.Location = new System.Drawing.Point(8, 38);
         this.label3.Name = "label3";
         this.label3.Size = new System.Drawing.Size(60, 13);
         this.label3.TabIndex = 18;
         this.label3.Text = "Completed:";
         // 
         // label2
         // 
         this.label2.AutoSize = true;
         this.label2.Location = new System.Drawing.Point(6, 16);
         this.label2.Name = "label2";
         this.label2.Size = new System.Drawing.Size(34, 13);
         this.label2.TabIndex = 17;
         this.label2.Text = "Total:";
         // 
         // NumCPUs
         // 
         this.NumCPUs.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
         this.NumCPUs.Location = new System.Drawing.Point(330, 7);
         this.NumCPUs.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
         this.NumCPUs.Name = "NumCPUs";
         this.NumCPUs.Size = new System.Drawing.Size(45, 20);
         this.NumCPUs.TabIndex = 15;
         this.NumCPUs.Value = new decimal(new int[] {
            1,
            0,
            0,
            0});
         // 
         // ToolStrip
         // 
         this.ToolStrip.CanOverflow = false;
         this.ToolStrip.Dock = System.Windows.Forms.DockStyle.None;
         this.ToolStrip.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden;
         this.ToolStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.StopButton,
            this.PauseButton});
         this.ToolStrip.Location = new System.Drawing.Point(0, 2);
         this.ToolStrip.Name = "ToolStrip";
         this.ToolStrip.RenderMode = System.Windows.Forms.ToolStripRenderMode.System;
         this.ToolStrip.Size = new System.Drawing.Size(49, 25);
         this.ToolStrip.TabIndex = 20;
         this.ToolStrip.Text = "toolStrip1";
         this.ToolStrip.ItemClicked += new System.Windows.Forms.ToolStripItemClickedEventHandler(this.OnButtonClick);
         // 
         // StopButton
         // 
         this.StopButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
         this.StopButton.Image = ((System.Drawing.Image)(resources.GetObject("StopButton.Image")));
         this.StopButton.ImageTransparentColor = System.Drawing.Color.Magenta;
         this.StopButton.Name = "StopButton";
         this.StopButton.Size = new System.Drawing.Size(23, 22);
         this.StopButton.Text = "toolStripButton2";
         this.StopButton.ToolTipText = "Stop simulations from running";
         // 
         // PauseButton
         // 
         this.PauseButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
         this.PauseButton.Image = ((System.Drawing.Image)(resources.GetObject("PauseButton.Image")));
         this.PauseButton.ImageTransparentColor = System.Drawing.Color.Magenta;
         this.PauseButton.Name = "PauseButton";
         this.PauseButton.Size = new System.Drawing.Size(23, 22);
         this.PauseButton.Text = "toolStripButton3";
         this.PauseButton.ToolTipText = "Pause running of simulations";
         // 
         // ShowDetailButton
         // 
         this.ShowDetailButton.AutoSize = true;
         this.ShowDetailButton.Location = new System.Drawing.Point(6, 150);
         this.ShowDetailButton.Name = "ShowDetailButton";
         this.ShowDetailButton.Size = new System.Drawing.Size(62, 13);
         this.ShowDetailButton.TabIndex = 21;
         this.ShowDetailButton.TabStop = true;
         this.ShowDetailButton.Text = "Show detail";
         this.ShowDetailButton.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.OnShowDetailClicked);
         // 
         // SimulationList
         // 
         this.SimulationList.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                     | System.Windows.Forms.AnchorStyles.Left)
                     | System.Windows.Forms.AnchorStyles.Right)));
         this.SimulationList.BorderStyle = System.Windows.Forms.BorderStyle.None;
         this.SimulationList.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.columnHeader1});
         this.SimulationList.GridLines = true;
         this.SimulationList.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.None;
         this.SimulationList.HideSelection = false;
         this.SimulationList.Items.AddRange(new System.Windows.Forms.ListViewItem[] {
            listViewItem1});
         this.SimulationList.Location = new System.Drawing.Point(9, 175);
         this.SimulationList.MultiSelect = false;
         this.SimulationList.Name = "SimulationList";
         this.SimulationList.ShowGroups = false;
         this.SimulationList.Size = new System.Drawing.Size(366, 254);
         this.SimulationList.SmallImageList = this.ImageList;
         this.SimulationList.TabIndex = 22;
         this.SimulationList.UseCompatibleStateImageBehavior = false;
         this.SimulationList.View = System.Windows.Forms.View.Details;
         this.SimulationList.MouseDoubleClick += new System.Windows.Forms.MouseEventHandler(this.OnMouseDoubleClick);
         // 
         // columnHeader1
         // 
         this.columnHeader1.Width = 365;
         // 
         // SimulationRunnerForm
         // 
         this.AllowDrop = true;
         this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
         this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
         this.ClientSize = new System.Drawing.Size(381, 441);
         this.Controls.Add(this.SimulationList);
         this.Controls.Add(this.ShowDetailButton);
         this.Controls.Add(this.ToolStrip);
         this.Controls.Add(this.groupBox2);
         this.Controls.Add(label1);
         this.Controls.Add(this.NumCPUs);
         this.Controls.Add(groupBox1);
         this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
         this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
         this.KeyPreview = true;
         this.MaximizeBox = false;
         this.Name = "SimulationRunnerForm";
         this.SizeGripStyle = System.Windows.Forms.SizeGripStyle.Hide;
         this.StartPosition = System.Windows.Forms.FormStartPosition.Manual;
         this.Text = "APSIM runs";
         this.Paint += new System.Windows.Forms.PaintEventHandler(this.OnPaint);
         this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.OnClosing);
         this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.OnKeyDown);
         groupBox1.ResumeLayout(false);
         this.groupBox2.ResumeLayout(false);
         this.groupBox2.PerformLayout();
         this.groupBox3.ResumeLayout(false);
         ((System.ComponentModel.ISupportInitialize)(this.NumCPUs)).EndInit();
         this.ToolStrip.ResumeLayout(false);
         this.ToolStrip.PerformLayout();
         this.ResumeLayout(false);
         this.PerformLayout();

         }

      #endregion

      private System.Windows.Forms.ImageList ImageList;
      private System.Diagnostics.PerformanceCounter PerformanceCounter;
      private System.Windows.Forms.Timer Timer1;
      private Steema.TeeChart.TChart PerformanceChart;
      private Steema.TeeChart.Styles.Area PerformanceSeries;
      private System.Windows.Forms.GroupBox groupBox2;
      private System.Windows.Forms.Label label5;
      private System.Windows.Forms.Label label4;
      private System.Windows.Forms.Label label3;
      private System.Windows.Forms.Label label2;
      private System.Windows.Forms.NumericUpDown NumCPUs;
      private System.Windows.Forms.Label Total;
      private System.Windows.Forms.Label NumberWithWarnings;
      private System.Windows.Forms.Label NumberWithErrors;
      private System.Windows.Forms.Label Completed;
      private System.Windows.Forms.GroupBox groupBox3;
      private UIBits.VerticalProgressBar ProgressBar;
      private System.Windows.Forms.ToolStrip ToolStrip;
      private System.Windows.Forms.ToolStripButton StopButton;
      private System.Windows.Forms.ToolStripButton PauseButton;
      private System.Windows.Forms.LinkLabel ShowDetailButton;
      private System.Windows.Forms.ListView SimulationList;
      private System.Windows.Forms.ColumnHeader columnHeader1;
      }
   }