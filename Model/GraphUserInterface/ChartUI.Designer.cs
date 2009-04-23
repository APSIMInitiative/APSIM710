namespace GraphUserInterface
    {
    partial class ChartUI
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
            this.Chart = new Steema.TeeChart.TChart();
            this.ColourDialog = new System.Windows.Forms.ColorDialog();
            this.PropertyGrid = new System.Windows.Forms.PropertyGrid();
            this.PropertyGridSplitter = new System.Windows.Forms.Splitter();
            this.SuspendLayout();
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
            this.Chart.Axes.Bottom.AxisPen.EndCap = System.Drawing.Drawing2D.LineCap.Round;
            this.Chart.Axes.Bottom.AxisPen.Width = 1;
            // 
            // 
            // 
            this.Chart.Axes.Bottom.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
            this.Chart.Axes.Bottom.Grid.Visible = false;
            this.Chart.Axes.Bottom.Grid.ZPosition = 0;
            // 
            // 
            // 
            this.Chart.Axes.Bottom.Labels.DateTimeFormat = "d/MM/yyyy";
            // 
            // 
            // 
            this.Chart.Axes.Bottom.Labels.Font.Name = "Tahoma";
            // 
            // 
            // 
            this.Chart.Axes.Bottom.Labels.Font.Shadow.Visible = false;
            this.Chart.Axes.Bottom.Labels.Font.Size = 9;
            this.Chart.Axes.Bottom.Labels.Separation = 80;
            // 
            // 
            // 
            this.Chart.Axes.Bottom.Labels.Shadow.Visible = false;
            this.Chart.Axes.Bottom.Labels.ValueFormat = "###0.###";
            // 
            // 
            // 
            this.Chart.Axes.Bottom.MinorTicks.Visible = false;
            // 
            // 
            // 
            this.Chart.Axes.Bottom.Ticks.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(0)))));
            // 
            // 
            // 
            this.Chart.Axes.Bottom.Title.Caption = "Date";
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Axes.Bottom.Title.Font.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(255)))));
            this.Chart.Axes.Bottom.Title.Font.Name = "Tahoma";
            // 
            // 
            // 
            this.Chart.Axes.Bottom.Title.Font.Shadow.Visible = false;
            this.Chart.Axes.Bottom.Title.Font.Size = 11;
            this.Chart.Axes.Bottom.Title.Lines = new string[] {
        "Date"};
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
            this.Chart.Axes.Depth.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
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
            this.Chart.Axes.DepthTop.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
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
            this.Chart.Axes.Left.AxisPen.EndCap = System.Drawing.Drawing2D.LineCap.Round;
            this.Chart.Axes.Left.AxisPen.Width = 1;
            // 
            // 
            // 
            this.Chart.Axes.Left.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
            this.Chart.Axes.Left.Grid.Visible = false;
            this.Chart.Axes.Left.Grid.ZPosition = 0;
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Axes.Left.Labels.Font.Name = "Tahoma";
            // 
            // 
            // 
            this.Chart.Axes.Left.Labels.Font.Shadow.Visible = false;
            this.Chart.Axes.Left.Labels.Font.Size = 9;
            this.Chart.Axes.Left.Labels.Separation = 80;
            // 
            // 
            // 
            this.Chart.Axes.Left.Labels.Shadow.Visible = false;
            this.Chart.Axes.Left.Labels.ValueFormat = "###0.###";
            // 
            // 
            // 
            this.Chart.Axes.Left.MinorTicks.Visible = false;
            // 
            // 
            // 
            this.Chart.Axes.Left.Ticks.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(0)))));
            // 
            // 
            // 
            this.Chart.Axes.Left.Title.Caption = "Mild";
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Axes.Left.Title.Font.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(255)))));
            this.Chart.Axes.Left.Title.Font.Name = "Tahoma";
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Axes.Left.Title.Font.Shadow.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(192)))));
            this.Chart.Axes.Left.Title.Font.Shadow.Visible = false;
            this.Chart.Axes.Left.Title.Font.Size = 11;
            this.Chart.Axes.Left.Title.Lines = new string[] {
        "Mild"};
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
            this.Chart.Axes.Right.AxisPen.EndCap = System.Drawing.Drawing2D.LineCap.Round;
            this.Chart.Axes.Right.AxisPen.Width = 1;
            // 
            // 
            // 
            this.Chart.Axes.Right.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
            this.Chart.Axes.Right.Grid.Visible = false;
            this.Chart.Axes.Right.Grid.ZPosition = 0;
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Axes.Right.Labels.Font.Name = "Tahoma";
            // 
            // 
            // 
            this.Chart.Axes.Right.Labels.Font.Shadow.Visible = false;
            this.Chart.Axes.Right.Labels.Font.Size = 9;
            this.Chart.Axes.Right.Labels.Separation = 80;
            // 
            // 
            // 
            this.Chart.Axes.Right.Labels.Shadow.Visible = false;
            this.Chart.Axes.Right.Labels.ValueFormat = "###0.###";
            // 
            // 
            // 
            this.Chart.Axes.Right.MinorTicks.Visible = false;
            // 
            // 
            // 
            this.Chart.Axes.Right.Ticks.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(0)))));
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Axes.Right.Title.Font.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(255)))));
            this.Chart.Axes.Right.Title.Font.Name = "Tahoma";
            // 
            // 
            // 
            this.Chart.Axes.Right.Title.Font.Shadow.Visible = false;
            this.Chart.Axes.Right.Title.Font.Size = 11;
            // 
            // 
            // 
            this.Chart.Axes.Right.Title.Shadow.Visible = false;
            this.Chart.Axes.Right.Visible = false;
            // 
            // 
            // 
            this.Chart.Axes.Top.Automatic = true;
            // 
            // 
            // 
            this.Chart.Axes.Top.AxisPen.EndCap = System.Drawing.Drawing2D.LineCap.Round;
            this.Chart.Axes.Top.AxisPen.Width = 1;
            // 
            // 
            // 
            this.Chart.Axes.Top.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
            this.Chart.Axes.Top.Grid.Visible = false;
            this.Chart.Axes.Top.Grid.ZPosition = 0;
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Axes.Top.Labels.Font.Name = "Tahoma";
            // 
            // 
            // 
            this.Chart.Axes.Top.Labels.Font.Shadow.Visible = false;
            this.Chart.Axes.Top.Labels.Font.Size = 9;
            this.Chart.Axes.Top.Labels.Separation = 80;
            // 
            // 
            // 
            this.Chart.Axes.Top.Labels.Shadow.Visible = false;
            this.Chart.Axes.Top.Labels.ValueFormat = "###0.###";
            // 
            // 
            // 
            this.Chart.Axes.Top.MinorTicks.Visible = false;
            // 
            // 
            // 
            this.Chart.Axes.Top.Ticks.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(0)))));
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Axes.Top.Title.Font.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(255)))));
            this.Chart.Axes.Top.Title.Font.Name = "Tahoma";
            // 
            // 
            // 
            this.Chart.Axes.Top.Title.Font.Shadow.Visible = false;
            this.Chart.Axes.Top.Title.Font.Size = 11;
            // 
            // 
            // 
            this.Chart.Axes.Top.Title.Shadow.Visible = false;
            this.Chart.Axes.Top.Visible = false;
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
            this.Chart.Header.Font.Name = "Tahoma";
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
            this.Chart.Legend.Alignment = Steema.TeeChart.LegendAlignments.Bottom;
            this.Chart.Legend.CheckBoxes = true;
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Legend.Font.Shadow.Visible = false;
            // 
            // 
            // 
            this.Chart.Legend.Shadow.Visible = false;
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
            this.Chart.Location = new System.Drawing.Point(0, 18);
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
            this.Chart.Panel.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            // 
            // 
            // 
            this.Chart.Panel.ImageBevel.Width = 1;
            // 
            // 
            // 
            this.Chart.Panel.Shadow.Visible = false;
            this.Chart.Size = new System.Drawing.Size(363, 506);
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
            this.Chart.TabIndex = 1;
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Walls.Back.AutoHide = false;
            // 
            // 
            // 
            this.Chart.Walls.Back.Shadow.Visible = false;
            // 
            // 
            // 
            this.Chart.Walls.Bottom.AutoHide = false;
            // 
            // 
            // 
            this.Chart.Walls.Bottom.Shadow.Visible = false;
            // 
            // 
            // 
            this.Chart.Walls.Left.AutoHide = false;
            // 
            // 
            // 
            this.Chart.Walls.Left.Shadow.Visible = false;
            // 
            // 
            // 
            this.Chart.Walls.Right.AutoHide = false;
            // 
            // 
            // 
            this.Chart.Walls.Right.Shadow.Visible = false;
            this.Chart.Walls.Visible = false;
            this.Chart.DoubleClick += new System.EventHandler(this.OnChartDoubleClick);
            // 
            // PropertyGrid
            // 
            this.PropertyGrid.Dock = System.Windows.Forms.DockStyle.Right;
            this.PropertyGrid.Location = new System.Drawing.Point(366, 18);
            this.PropertyGrid.Name = "PropertyGrid";
            this.PropertyGrid.SelectedObject = this;
            this.PropertyGrid.Size = new System.Drawing.Size(211, 506);
            this.PropertyGrid.TabIndex = 2;
            this.PropertyGrid.Visible = false;
            // 
            // PropertyGridSplitter
            // 
            this.PropertyGridSplitter.Dock = System.Windows.Forms.DockStyle.Right;
            this.PropertyGridSplitter.Location = new System.Drawing.Point(363, 18);
            this.PropertyGridSplitter.Name = "PropertyGridSplitter";
            this.PropertyGridSplitter.Size = new System.Drawing.Size(3, 506);
            this.PropertyGridSplitter.TabIndex = 3;
            this.PropertyGridSplitter.TabStop = false;
            this.PropertyGridSplitter.Visible = false;
            // 
            // ChartUI
            // 
            this.Controls.Add(this.Chart);
            this.Controls.Add(this.PropertyGridSplitter);
            this.Controls.Add(this.PropertyGrid);
            this.Name = "ChartUI";
            this.Size = new System.Drawing.Size(577, 524);
            this.Controls.SetChildIndex(this.PropertyGrid, 0);
            this.Controls.SetChildIndex(this.PropertyGridSplitter, 0);
            this.Controls.SetChildIndex(this.Chart, 0);
            this.ResumeLayout(false);

            }

        #endregion

        private System.Windows.Forms.ColorDialog ColourDialog;
       public Steema.TeeChart.TChart Chart;
       private System.Windows.Forms.PropertyGrid PropertyGrid;
       private System.Windows.Forms.Splitter PropertyGridSplitter;

        }
    }
