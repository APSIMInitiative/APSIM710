namespace CSUserInterface
   {
   partial class Plant2Function
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
         System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Plant2Function));
         this.splitter1 = new System.Windows.Forms.Splitter();
         this.Chart = new Steema.TeeChart.TChart();
         this.Line = new Steema.TeeChart.Styles.Line();
         this.panel1 = new System.Windows.Forms.Panel();
         this.Properties = new VBUserInterface.GenericUI();
         this.Grid = new System.Windows.Forms.DataGridView();
         this.splitter2 = new System.Windows.Forms.Splitter();
         this.panel1.SuspendLayout();
         ((System.ComponentModel.ISupportInitialize)(this.Grid)).BeginInit();
         this.SuspendLayout();
         // 
         // splitter1
         // 
         this.splitter1.Location = new System.Drawing.Point(278, 0);
         this.splitter1.Name = "splitter1";
         this.splitter1.Size = new System.Drawing.Size(3, 215);
         this.splitter1.TabIndex = 4;
         this.splitter1.TabStop = false;
         // 
         // Chart
         // 
         // 
         // 
         // 
         this.Chart.Aspect.ColorPaletteIndex = 3;
         this.Chart.Aspect.ElevationFloat = 345;
         this.Chart.Aspect.RotationFloat = 345;
         this.Chart.Aspect.View3D = false;
         // 
         // 
         // 
         // 
         // 
         // 
         this.Chart.Axes.Bottom.Automatic = true;
         // 
         // 
         // 
         this.Chart.Axes.Bottom.AxisPen.Width = 1;
         // 
         // 
         // 
         this.Chart.Axes.Bottom.Grid.Color = System.Drawing.Color.Gray;
         this.Chart.Axes.Bottom.Grid.Style = System.Drawing.Drawing2D.DashStyle.Solid;
         this.Chart.Axes.Bottom.Grid.Visible = false;
         this.Chart.Axes.Bottom.Grid.ZPosition = 0;
         // 
         // 
         // 
         this.Chart.Axes.Bottom.Labels.CustomSize = 30;
         // 
         // 
         // 
         // 
         // 
         // 
         this.Chart.Axes.Bottom.Labels.Font.Shadow.Visible = false;
         this.Chart.Axes.Bottom.Labels.Font.Size = 11;
         // 
         // 
         // 
         this.Chart.Axes.Bottom.Labels.Shadow.Visible = false;
         this.Chart.Axes.Bottom.Labels.ValueFormat = "#,##0.####";
         // 
         // 
         // 
         this.Chart.Axes.Bottom.MinorTicks.Visible = false;
         // 
         // 
         // 
         this.Chart.Axes.Bottom.Ticks.Color = System.Drawing.Color.Black;
         this.Chart.Axes.Bottom.Ticks.Length = 2;
         // 
         // 
         // 
         this.Chart.Axes.Bottom.TicksInner.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.Chart.Axes.Bottom.Title.Font.Shadow.Visible = false;
         this.Chart.Axes.Bottom.Title.Font.Size = 12;
         this.Chart.Axes.Bottom.Title.Lines = new string[] {
        ""};
         // 
         // 
         // 
         this.Chart.Axes.Bottom.Title.Shadow.Visible = false;
         // 
         // 
         // 
         this.Chart.Axes.Depth.Automatic = true;
         // 
         // 
         // 
         this.Chart.Axes.Depth.AxisPen.Width = 1;
         // 
         // 
         // 
         this.Chart.Axes.Depth.Grid.Color = System.Drawing.Color.Gray;
         this.Chart.Axes.Depth.Grid.Style = System.Drawing.Drawing2D.DashStyle.Solid;
         this.Chart.Axes.Depth.Grid.ZPosition = 0;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.Chart.Axes.Depth.Labels.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.Chart.Axes.Depth.Labels.Shadow.Visible = false;
         // 
         // 
         // 
         this.Chart.Axes.Depth.MinorTicks.Visible = false;
         // 
         // 
         // 
         this.Chart.Axes.Depth.Ticks.Color = System.Drawing.Color.Black;
         this.Chart.Axes.Depth.Ticks.Length = 2;
         // 
         // 
         // 
         this.Chart.Axes.Depth.TicksInner.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.Chart.Axes.Depth.Title.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.Chart.Axes.Depth.Title.Shadow.Visible = false;
         // 
         // 
         // 
         this.Chart.Axes.DepthTop.Automatic = true;
         // 
         // 
         // 
         this.Chart.Axes.DepthTop.AxisPen.Width = 1;
         // 
         // 
         // 
         this.Chart.Axes.DepthTop.Grid.Color = System.Drawing.Color.Gray;
         this.Chart.Axes.DepthTop.Grid.Style = System.Drawing.Drawing2D.DashStyle.Solid;
         this.Chart.Axes.DepthTop.Grid.ZPosition = 0;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.Chart.Axes.DepthTop.Labels.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.Chart.Axes.DepthTop.Labels.Shadow.Visible = false;
         // 
         // 
         // 
         this.Chart.Axes.DepthTop.MinorTicks.Visible = false;
         // 
         // 
         // 
         this.Chart.Axes.DepthTop.Ticks.Color = System.Drawing.Color.Black;
         this.Chart.Axes.DepthTop.Ticks.Length = 2;
         // 
         // 
         // 
         this.Chart.Axes.DepthTop.TicksInner.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.Chart.Axes.DepthTop.Title.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.Chart.Axes.DepthTop.Title.Shadow.Visible = false;
         // 
         // 
         // 
         this.Chart.Axes.Left.Automatic = true;
         // 
         // 
         // 
         this.Chart.Axes.Left.AxisPen.Width = 1;
         // 
         // 
         // 
         this.Chart.Axes.Left.Grid.Color = System.Drawing.Color.Gray;
         this.Chart.Axes.Left.Grid.Style = System.Drawing.Drawing2D.DashStyle.Solid;
         this.Chart.Axes.Left.Grid.Visible = false;
         this.Chart.Axes.Left.Grid.ZPosition = 0;
         // 
         // 
         // 
         this.Chart.Axes.Left.Labels.CustomSize = 40;
         // 
         // 
         // 
         // 
         // 
         // 
         this.Chart.Axes.Left.Labels.Font.Shadow.Visible = false;
         this.Chart.Axes.Left.Labels.Font.Size = 11;
         // 
         // 
         // 
         this.Chart.Axes.Left.Labels.Shadow.Visible = false;
         this.Chart.Axes.Left.Labels.ValueFormat = "###0.#####";
         this.Chart.Axes.Left.MaximumOffset = 5;
         // 
         // 
         // 
         this.Chart.Axes.Left.MinorTicks.Visible = false;
         // 
         // 
         // 
         this.Chart.Axes.Left.Ticks.Color = System.Drawing.Color.Black;
         this.Chart.Axes.Left.Ticks.Length = 2;
         // 
         // 
         // 
         this.Chart.Axes.Left.TicksInner.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.Chart.Axes.Left.Title.Font.Shadow.Visible = false;
         this.Chart.Axes.Left.Title.Font.Size = 12;
         this.Chart.Axes.Left.Title.Lines = new string[] {
        ""};
         // 
         // 
         // 
         this.Chart.Axes.Left.Title.Shadow.Visible = false;
         // 
         // 
         // 
         this.Chart.Axes.Right.Automatic = true;
         // 
         // 
         // 
         this.Chart.Axes.Right.AxisPen.Width = 1;
         // 
         // 
         // 
         this.Chart.Axes.Right.Grid.Color = System.Drawing.Color.Gray;
         this.Chart.Axes.Right.Grid.Style = System.Drawing.Drawing2D.DashStyle.Solid;
         this.Chart.Axes.Right.Grid.ZPosition = 0;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.Chart.Axes.Right.Labels.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.Chart.Axes.Right.Labels.Shadow.Visible = false;
         // 
         // 
         // 
         this.Chart.Axes.Right.MinorTicks.Visible = false;
         // 
         // 
         // 
         this.Chart.Axes.Right.Ticks.Color = System.Drawing.Color.Black;
         this.Chart.Axes.Right.Ticks.Length = 2;
         // 
         // 
         // 
         this.Chart.Axes.Right.TicksInner.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.Chart.Axes.Right.Title.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.Chart.Axes.Right.Title.Shadow.Visible = false;
         // 
         // 
         // 
         this.Chart.Axes.Top.Automatic = true;
         // 
         // 
         // 
         this.Chart.Axes.Top.AxisPen.Width = 1;
         // 
         // 
         // 
         this.Chart.Axes.Top.Grid.Color = System.Drawing.Color.Gray;
         this.Chart.Axes.Top.Grid.Style = System.Drawing.Drawing2D.DashStyle.Solid;
         this.Chart.Axes.Top.Grid.ZPosition = 0;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.Chart.Axes.Top.Labels.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.Chart.Axes.Top.Labels.Shadow.Visible = false;
         // 
         // 
         // 
         this.Chart.Axes.Top.MinorTicks.Visible = false;
         // 
         // 
         // 
         this.Chart.Axes.Top.Ticks.Color = System.Drawing.Color.Black;
         this.Chart.Axes.Top.Ticks.Length = 2;
         // 
         // 
         // 
         this.Chart.Axes.Top.TicksInner.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.Chart.Axes.Top.Title.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.Chart.Axes.Top.Title.Shadow.Visible = false;
         this.Chart.BackColor = System.Drawing.SystemColors.Window;
         this.Chart.Cursor = System.Windows.Forms.Cursors.Default;
         this.Chart.Dock = System.Windows.Forms.DockStyle.Fill;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.Chart.Footer.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.Chart.Footer.Shadow.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         this.Chart.Header.Brush.Color = System.Drawing.Color.White;
         // 
         // 
         // 
         this.Chart.Header.Gradient.EndColor = System.Drawing.Color.Gray;
         this.Chart.Header.Gradient.MiddleColor = System.Drawing.Color.Empty;
         this.Chart.Header.Gradient.StartColor = System.Drawing.Color.White;
         this.Chart.Header.Gradient.Visible = true;
         // 
         // 
         // 
         // 
         // 
         // 
         this.Chart.Header.Font.Brush.Color = System.Drawing.Color.Black;
         // 
         // 
         // 
         this.Chart.Header.Font.Shadow.Visible = false;
         this.Chart.Header.Font.Size = 12;
         this.Chart.Header.Lines = new string[] {
        ""};
         // 
         // 
         // 
         this.Chart.Header.Shadow.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         this.Chart.Legend.DividingLines.Visible = true;
         // 
         // 
         // 
         // 
         // 
         // 
         this.Chart.Legend.Font.Shadow.Visible = false;
         this.Chart.Legend.Font.Size = 10;
         // 
         // 
         // 
         // 
         // 
         // 
         this.Chart.Legend.Shadow.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(0)))));
         this.Chart.Legend.Shadow.Height = 0;
         this.Chart.Legend.Shadow.Width = 0;
         // 
         // 
         // 
         // 
         // 
         // 
         this.Chart.Legend.Title.Font.Bold = true;
         // 
         // 
         // 
         this.Chart.Legend.Title.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.Chart.Legend.Title.Pen.Visible = false;
         // 
         // 
         // 
         this.Chart.Legend.Title.Shadow.Visible = false;
         this.Chart.Legend.Transparent = true;
         this.Chart.Legend.Visible = false;
         this.Chart.Location = new System.Drawing.Point(0, 231);
         this.Chart.Name = "Chart";
         // 
         // 
         // 
         // 
         // 
         // 
         this.Chart.Panel.Bevel.Outer = Steema.TeeChart.Drawing.BevelStyles.None;
         // 
         // 
         // 
         this.Chart.Panel.Brush.Color = System.Drawing.Color.White;
         // 
         // 
         // 
         this.Chart.Panel.Gradient.EndColor = System.Drawing.Color.Yellow;
         this.Chart.Panel.Gradient.MiddleColor = System.Drawing.Color.Empty;
         this.Chart.Panel.Gradient.StartColor = System.Drawing.Color.White;
         // 
         // 
         // 
         this.Chart.Panel.ImageBevel.Width = 1;
         // 
         // 
         // 
         this.Chart.Panel.Shadow.Height = 0;
         this.Chart.Panel.Shadow.Visible = false;
         this.Chart.Panel.Shadow.Width = 0;
         this.Chart.Series.Add(this.Line);
         this.Chart.Size = new System.Drawing.Size(655, 310);
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.Chart.SubFooter.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.Chart.SubFooter.Shadow.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         // 
         this.Chart.SubHeader.Font.Shadow.Visible = false;
         // 
         // 
         // 
         this.Chart.SubHeader.Shadow.Visible = false;
         this.Chart.TabIndex = 5;
         // 
         // 
         // 
         // 
         // 
         // 
         this.Chart.Walls.Back.ApplyDark = false;
         this.Chart.Walls.Back.AutoHide = false;
         // 
         // 
         // 
         this.Chart.Walls.Back.Brush.Color = System.Drawing.Color.White;
         // 
         // 
         // 
         this.Chart.Walls.Back.Shadow.Visible = false;
         this.Chart.Walls.Back.Size = 8;
         this.Chart.Walls.Back.Transparent = false;
         // 
         // 
         // 
         this.Chart.Walls.Bottom.ApplyDark = false;
         this.Chart.Walls.Bottom.AutoHide = false;
         // 
         // 
         // 
         this.Chart.Walls.Bottom.Shadow.Visible = false;
         this.Chart.Walls.Bottom.Size = 8;
         // 
         // 
         // 
         this.Chart.Walls.Left.ApplyDark = false;
         this.Chart.Walls.Left.AutoHide = false;
         // 
         // 
         // 
         this.Chart.Walls.Left.Brush.Color = System.Drawing.Color.White;
         // 
         // 
         // 
         this.Chart.Walls.Left.Shadow.Visible = false;
         this.Chart.Walls.Left.Size = 8;
         // 
         // 
         // 
         this.Chart.Walls.Right.ApplyDark = false;
         this.Chart.Walls.Right.AutoHide = false;
         // 
         // 
         // 
         this.Chart.Walls.Right.Brush.Color = System.Drawing.Color.White;
         // 
         // 
         // 
         this.Chart.Walls.Right.Shadow.Visible = false;
         this.Chart.Walls.Right.Size = 8;
         this.Chart.Walls.Visible = false;
         // 
         // Line
         // 
         // 
         // 
         // 
         this.Line.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(192)))));
         // 
         // 
         // 
         this.Line.LinePen.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(115)))));
         this.Line.LinePen.Width = 2;
         // 
         // 
         // 
         // 
         // 
         // 
         this.Line.Marks.Callout.ArrowHead = Steema.TeeChart.Styles.ArrowHeadStyles.None;
         this.Line.Marks.Callout.ArrowHeadSize = 8;
         // 
         // 
         // 
         this.Line.Marks.Callout.Brush.Color = System.Drawing.Color.Black;
         this.Line.Marks.Callout.Distance = 0;
         this.Line.Marks.Callout.Draw3D = false;
         this.Line.Marks.Callout.Length = 10;
         this.Line.Marks.Callout.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
         // 
         // 
         // 
         // 
         // 
         // 
         this.Line.Marks.Font.Shadow.Visible = false;
         // 
         // 
         // 
         // 
         // 
         // 
         this.Line.Pointer.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(192)))));
         this.Line.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
         this.Line.Title = "Line";
         // 
         // 
         // 
         this.Line.XValues.DataMember = "X";
         this.Line.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending;
         // 
         // 
         // 
         this.Line.YValues.DataMember = "Y";
         // 
         // panel1
         // 
         this.panel1.Controls.Add(this.splitter1);
         this.panel1.Controls.Add(this.Properties);
         this.panel1.Controls.Add(this.Grid);
         this.panel1.Dock = System.Windows.Forms.DockStyle.Top;
         this.panel1.Location = new System.Drawing.Point(0, 16);
         this.panel1.Name = "panel1";
         this.panel1.Size = new System.Drawing.Size(655, 215);
         this.panel1.TabIndex = 6;
         // 
         // Properties
         // 
         this.Properties.AutoScroll = true;
         this.Properties.BackColor = System.Drawing.SystemColors.Window;
         this.Properties.Dock = System.Windows.Forms.DockStyle.Left;
         this.Properties.HelpText = "";
         this.Properties.Location = new System.Drawing.Point(0, 0);
         this.Properties.Name = "Properties";
         this.Properties.Size = new System.Drawing.Size(278, 215);
         this.Properties.TabIndex = 3;
         // 
         // Grid
         // 
         this.Grid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
         this.Grid.Dock = System.Windows.Forms.DockStyle.Fill;
         this.Grid.Location = new System.Drawing.Point(0, 0);
         this.Grid.Name = "Grid";
         this.Grid.Size = new System.Drawing.Size(655, 215);
         this.Grid.TabIndex = 3;
         // 
         // splitter2
         // 
         this.splitter2.Dock = System.Windows.Forms.DockStyle.Top;
         this.splitter2.Location = new System.Drawing.Point(0, 231);
         this.splitter2.Name = "splitter2";
         this.splitter2.Size = new System.Drawing.Size(655, 3);
         this.splitter2.TabIndex = 7;
         this.splitter2.TabStop = false;
         // 
         // Plant2Function
         // 
         this.Controls.Add(this.splitter2);
         this.Controls.Add(this.Chart);
         this.Controls.Add(this.panel1);
         this.Name = "Plant2Function";
         this.Controls.SetChildIndex(this.MyHelpLabel, 0);
         this.Controls.SetChildIndex(this.panel1, 0);
         this.Controls.SetChildIndex(this.Chart, 0);
         this.Controls.SetChildIndex(this.splitter2, 0);
         this.panel1.ResumeLayout(false);
         ((System.ComponentModel.ISupportInitialize)(this.Grid)).EndInit();
         this.ResumeLayout(false);

         }

      #endregion

      private System.Windows.Forms.Splitter splitter1;
      private Steema.TeeChart.TChart Chart;
      private Steema.TeeChart.Styles.Line Line;
      private System.Windows.Forms.Panel panel1;
      private VBUserInterface.GenericUI Properties;
      private System.Windows.Forms.DataGridView Grid;
      private System.Windows.Forms.Splitter splitter2;
      }
   }
