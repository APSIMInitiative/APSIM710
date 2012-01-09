namespace Graph
   {
   partial class GraphUI2
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
            this.Chart = new Steema.TeeChart.TChart();
            this.PopupMenuStrip = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.PropertiesMenu = new System.Windows.Forms.ToolStripMenuItem();
            this.PopupMenuStrip.SuspendLayout();
            this.SuspendLayout();
            // 
            // MyHelpLabel
            // 
            this.MyHelpLabel.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.MyHelpLabel.Size = new System.Drawing.Size(680, 20);
            // 
            // Chart
            // 
            // 
            // 
            // 
            this.Chart.Aspect.ColorPaletteIndex = 3;
            this.Chart.Aspect.View3D = false;
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Axes.Bottom.AxisPen.EndCap = System.Drawing.Drawing2D.LineCap.Round;
            this.Chart.Axes.Bottom.AxisPen.Width = 1;
            // 
            // 
            // 
            this.Chart.Axes.Bottom.Grid.Visible = false;
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Axes.Bottom.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.Chart.Axes.Bottom.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.Chart.Axes.Bottom.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.Chart.Axes.Bottom.Labels.Bevel.StringColorTwo = "FF808080";
            this.Chart.Axes.Bottom.Labels.DateTimeFormat = "d/MM/yyyy";
            // 
            // 
            // 
            this.Chart.Axes.Bottom.Labels.Font.Name = "Tahoma";
            this.Chart.Axes.Bottom.Labels.Font.Size = 9;
            this.Chart.Axes.Bottom.Labels.Font.SizeFloat = 9F;
            this.Chart.Axes.Bottom.Labels.Separation = 80;
            this.Chart.Axes.Bottom.Labels.ValueFormat = "###0.###";
            this.Chart.Axes.Bottom.MaximumOffset = 10;
            this.Chart.Axes.Bottom.MinimumOffset = 10;
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
            // 
            // 
            // 
            this.Chart.Axes.Bottom.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.Chart.Axes.Bottom.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.Chart.Axes.Bottom.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.Chart.Axes.Bottom.Title.Bevel.StringColorTwo = "FF808080";
            this.Chart.Axes.Bottom.Title.Caption = "[]";
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Axes.Bottom.Title.Font.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(255)))));
            this.Chart.Axes.Bottom.Title.Font.Name = "Tahoma";
            this.Chart.Axes.Bottom.Title.Font.Size = 11;
            this.Chart.Axes.Bottom.Title.Font.SizeFloat = 11F;
            this.Chart.Axes.Bottom.Title.Lines = new string[] {
        "[]"};
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Axes.Depth.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.Chart.Axes.Depth.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.Chart.Axes.Depth.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.Chart.Axes.Depth.Labels.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Axes.Depth.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.Chart.Axes.Depth.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.Chart.Axes.Depth.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.Chart.Axes.Depth.Title.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Axes.DepthTop.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.Chart.Axes.DepthTop.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.Chart.Axes.DepthTop.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.Chart.Axes.DepthTop.Labels.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Axes.DepthTop.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.Chart.Axes.DepthTop.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.Chart.Axes.DepthTop.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.Chart.Axes.DepthTop.Title.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Axes.Left.AxisPen.EndCap = System.Drawing.Drawing2D.LineCap.Round;
            this.Chart.Axes.Left.AxisPen.Width = 1;
            // 
            // 
            // 
            this.Chart.Axes.Left.Grid.Visible = false;
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Axes.Left.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.Chart.Axes.Left.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.Chart.Axes.Left.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.Chart.Axes.Left.Labels.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            this.Chart.Axes.Left.Labels.Font.Name = "Tahoma";
            this.Chart.Axes.Left.Labels.Font.Size = 9;
            this.Chart.Axes.Left.Labels.Font.SizeFloat = 9F;
            this.Chart.Axes.Left.Labels.Separation = 80;
            this.Chart.Axes.Left.Labels.ValueFormat = "###0.###";
            this.Chart.Axes.Left.MaximumOffset = 10;
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
            // 
            // 
            // 
            this.Chart.Axes.Left.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.Chart.Axes.Left.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.Chart.Axes.Left.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.Chart.Axes.Left.Title.Bevel.StringColorTwo = "FF808080";
            this.Chart.Axes.Left.Title.Caption = "[]";
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
            this.Chart.Axes.Left.Title.Font.Size = 11;
            this.Chart.Axes.Left.Title.Font.SizeFloat = 11F;
            this.Chart.Axes.Left.Title.Lines = new string[] {
        "[]"};
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Axes.Right.AxisPen.EndCap = System.Drawing.Drawing2D.LineCap.Round;
            this.Chart.Axes.Right.AxisPen.Width = 1;
            // 
            // 
            // 
            this.Chart.Axes.Right.Grid.Visible = false;
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Axes.Right.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.Chart.Axes.Right.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.Chart.Axes.Right.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.Chart.Axes.Right.Labels.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            this.Chart.Axes.Right.Labels.Font.Name = "Tahoma";
            this.Chart.Axes.Right.Labels.Font.Size = 9;
            this.Chart.Axes.Right.Labels.Font.SizeFloat = 9F;
            this.Chart.Axes.Right.Labels.Separation = 80;
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
            this.Chart.Axes.Right.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.Chart.Axes.Right.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.Chart.Axes.Right.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.Chart.Axes.Right.Title.Bevel.StringColorTwo = "FF808080";
            this.Chart.Axes.Right.Title.Caption = "[]";
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Axes.Right.Title.Font.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(255)))));
            this.Chart.Axes.Right.Title.Font.Name = "Tahoma";
            this.Chart.Axes.Right.Title.Font.Size = 11;
            this.Chart.Axes.Right.Title.Font.SizeFloat = 11F;
            this.Chart.Axes.Right.Title.Lines = new string[] {
        "[]"};
            this.Chart.Axes.Right.Visible = false;
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Axes.Top.AxisPen.EndCap = System.Drawing.Drawing2D.LineCap.Round;
            this.Chart.Axes.Top.AxisPen.Width = 1;
            // 
            // 
            // 
            this.Chart.Axes.Top.Grid.Visible = false;
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Axes.Top.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.Chart.Axes.Top.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.Chart.Axes.Top.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.Chart.Axes.Top.Labels.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            this.Chart.Axes.Top.Labels.Font.Name = "Tahoma";
            this.Chart.Axes.Top.Labels.Font.Size = 9;
            this.Chart.Axes.Top.Labels.Font.SizeFloat = 9F;
            this.Chart.Axes.Top.Labels.Separation = 80;
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
            this.Chart.Axes.Top.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.Chart.Axes.Top.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.Chart.Axes.Top.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.Chart.Axes.Top.Title.Bevel.StringColorTwo = "FF808080";
            this.Chart.Axes.Top.Title.Caption = "[]";
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Axes.Top.Title.Font.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(255)))));
            this.Chart.Axes.Top.Title.Font.Name = "Tahoma";
            this.Chart.Axes.Top.Title.Font.Size = 11;
            this.Chart.Axes.Top.Title.Font.SizeFloat = 11F;
            this.Chart.Axes.Top.Title.Lines = new string[] {
        "[]"};
            this.Chart.Axes.Top.Visible = false;
            this.Chart.BackColor = System.Drawing.SystemColors.Window;
            this.Chart.ContextMenuStrip = this.PopupMenuStrip;
            this.Chart.Cursor = System.Windows.Forms.Cursors.Default;
            this.Chart.Dock = System.Windows.Forms.DockStyle.Fill;
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Footer.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.Chart.Footer.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.Chart.Footer.Bevel.StringColorOne = "FFFFFFFF";
            this.Chart.Footer.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Header.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.Chart.Header.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.Chart.Header.Bevel.StringColorOne = "FFFFFFFF";
            this.Chart.Header.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            this.Chart.Header.Font.Name = "Tahoma";
            this.Chart.Header.Font.Size = 12;
            this.Chart.Header.Font.SizeFloat = 12F;
            this.Chart.Header.Lines = new string[] {
        ""};
            // 
            // 
            // 
            this.Chart.Legend.Alignment = Steema.TeeChart.LegendAlignments.Bottom;
            // 
            // 
            // 
            this.Chart.Legend.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.Chart.Legend.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.Chart.Legend.Bevel.StringColorOne = "FFFFFFFF";
            this.Chart.Legend.Bevel.StringColorTwo = "FF808080";
            this.Chart.Legend.CheckBoxes = true;
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
            this.Chart.Legend.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.Chart.Legend.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.Chart.Legend.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.Chart.Legend.Title.Bevel.StringColorTwo = "FF808080";
            this.Chart.Legend.Transparent = true;
            this.Chart.Location = new System.Drawing.Point(0, 0);
            this.Chart.Margin = new System.Windows.Forms.Padding(4);
            this.Chart.Name = "Chart";
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Panel.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.Chart.Panel.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.Chart.Panel.Bevel.Outer = Steema.TeeChart.Drawing.BevelStyles.None;
            this.Chart.Panel.Bevel.StringColorOne = "FFFFFFFF";
            this.Chart.Panel.Bevel.StringColorTwo = "FF808080";
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
            this.Chart.Panning.MouseButton = System.Windows.Forms.MouseButtons.Middle;
            this.Chart.Size = new System.Drawing.Size(680, 575);
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.SubFooter.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.Chart.SubFooter.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.Chart.SubFooter.Bevel.StringColorOne = "FFFFFFFF";
            this.Chart.SubFooter.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.SubHeader.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.Chart.SubHeader.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.Chart.SubHeader.Bevel.StringColorOne = "FFFFFFFF";
            this.Chart.SubHeader.Bevel.StringColorTwo = "FF808080";
            this.Chart.TabIndex = 4;
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Walls.Back.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.Chart.Walls.Back.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.Chart.Walls.Back.Bevel.StringColorOne = "FFFFFFFF";
            this.Chart.Walls.Back.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Walls.Bottom.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.Chart.Walls.Bottom.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.Chart.Walls.Bottom.Bevel.StringColorOne = "FFFFFFFF";
            this.Chart.Walls.Bottom.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Walls.Left.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.Chart.Walls.Left.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.Chart.Walls.Left.Bevel.StringColorOne = "FFFFFFFF";
            this.Chart.Walls.Left.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.Chart.Walls.Right.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.Chart.Walls.Right.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.Chart.Walls.Right.Bevel.StringColorOne = "FFFFFFFF";
            this.Chart.Walls.Right.Bevel.StringColorTwo = "FF808080";
            this.Chart.Walls.Visible = false;
            // 
            // 
            // 
            this.Chart.Zoom.Animated = true;
            this.Chart.Zoom.AnimatedSteps = 4;
            // 
            // PopupMenuStrip
            // 
            this.PopupMenuStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.PropertiesMenu});
            this.PopupMenuStrip.Name = "contextMenuStrip1";
            this.PopupMenuStrip.Size = new System.Drawing.Size(146, 28);
            // 
            // PropertiesMenu
            // 
            this.PropertiesMenu.Name = "PropertiesMenu";
            this.PropertiesMenu.Size = new System.Drawing.Size(145, 24);
            this.PropertiesMenu.Text = "Properties";
            this.PropertiesMenu.Click += new System.EventHandler(this.OnPropertiesClick);
            // 
            // GraphUI2
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 16F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.Chart);
            this.Margin = new System.Windows.Forms.Padding(4);
            this.Name = "GraphUI2";
            this.Size = new System.Drawing.Size(680, 575);
            this.Controls.SetChildIndex(this.Chart, 0);
            this.Controls.SetChildIndex(this.MyHelpLabel, 0);
            this.PopupMenuStrip.ResumeLayout(false);
            this.ResumeLayout(false);

         }

      #endregion

      private System.Windows.Forms.ContextMenuStrip PopupMenuStrip;
      private System.Windows.Forms.ToolStripMenuItem PropertiesMenu;
      public Steema.TeeChart.TChart Chart;

      }
   }